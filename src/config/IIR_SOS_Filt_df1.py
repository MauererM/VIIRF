"""
    MIT License

    Copyright (c) 2017 Mario Mauerer

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

    VIIRF - Versatile IIR Filter

    This file implements direct-form 1-based IIR filters with
    cascaded second-order sections.

    The nomenclature used is somewhat related to IIR_Config.py.

    Python: 3.5.2
    NOTE: The behavior of e.g., integers, longs or integer divisions differs from python 2 and 3!
    Python 3 is preferred/required due to the infinite integer-precision provided by python 3.
    Do NOT use NUMPY-datatypes for filter calculations. They are limited to 64 bit and this is quickly not enough
    for high-precision / low-noise filters.

"""

import numpy as np


class SOS_Casc_df1_Float:
    """ This class implements the direct-form 1 implementation of an IIR filter made from cascaded SOS.
        Floating-point coefficient/data is used.
    """

    def __init__(self, sos, g, gain_input, sosgain_en, finalgain_en):
        """ Constructor:
            :param sos: list of lists containing the SOS coefficients [b0, b1, b2, 1, a1, a2]
            :param g: list of the section/filter gains.
                        Length depends on sosgain_en and finalgain_en (See IIR_Config.py)
            :param gain_input: Filter input gain; must be power of two
            :param sosgain_en: If set to true, the section-gains are enabled
            :param finalgain_en: If set to true, the filter's final output gain is enabled
        """
        self.__sos = [[float(y) for y in x] for x in sos]  # Convert to float, just to be sure.
        self.__g = [float(x) for x in g]
        self.__gain_input = float(gain_input)
        self.__sosgain_en = bool(sosgain_en)
        self.__finalgain_en = bool(finalgain_en)
        self.__sos_np = np.array(sos)  # Create numpy-array to facilitate certain operations (like allowing shape)
        self.__numsec, _ = self.__sos_np.shape
        # Section registers of direct-form 1. Each section has a set of registers. They store the filter's state.
        self.__b0Reg = np.zeros(self.__numsec, dtype=np.float64)
        self.__b1Reg = np.zeros(self.__numsec, dtype=np.float64)
        self.__b2Reg = np.zeros(self.__numsec, dtype=np.float64)
        self.__a1Reg = np.zeros(self.__numsec, dtype=np.float64)
        self.__a2Reg = np.zeros(self.__numsec, dtype=np.float64)
        self.__satReg = np.zeros(self.__numsec, dtype=np.float64)
        self.__outReg = np.zeros(self.__numsec, dtype=np.float64)

    def update_filter(self, input):
        """ Updates the filter with a new input and returns the new output sample. """

        input = float(input)  # Make sure to stay with floats
        # Apply the filter's input gain:
        input = input * self.__gain_input

        # Update the filter's states and calculate the new output:
        # First, iterate through all sections and calculate the direct-form 1's new states.
        # Then, apply the output gains, if applicable.
        for i in range(0, self.__numsec):
            # Shift each section register:
            self.__a2Reg[i] = self.__a1Reg[i]
            self.__a1Reg[i] = self.__satReg[i]
            self.__b2Reg[i] = self.__b1Reg[i]
            self.__b1Reg[i] = self.__b0Reg[i]
            # The input register is either the filter input or the output of the previous section,
            # depending on the current section:
            if i == 0:
                self.__b0Reg[i] = input
            else:
                self.__b0Reg[i] = self.__outReg[i - 1]

            # Do the MulAccs of the direct-form 1: Careful with the sign of the a1, a2 coefficients.
            self.__satReg[i] = self.__b0Reg[i] * self.__sos[i][0] + \
                               self.__b1Reg[i] * self.__sos[i][1] + \
                               self.__b2Reg[i] * self.__sos[i][2] - \
                               self.__a1Reg[i] * self.__sos[i][4] - \
                               self.__a2Reg[i] * self.__sos[i][5]
            # If the section-gain is enabled, apply it:
            if self.__sosgain_en is True:
                self.__outReg[i] = self.__satReg[i] * self.__g[i]
            else:
                self.__outReg[i] = self.__satReg[i]

        # Once iterated through all the sections: Apply the filter's output gain, if enabled:
        if self.__finalgain_en is True:
            finalout = self.__outReg[self.__numsec - 1] * self.__g[-1]  # Last entry in G stores the final output gain
        else:
            # No final gain enabled: directly output the content of the last section:
            finalout = self.__outReg[self.__numsec - 1]
        # Return the filter's output:
        return finalout


class SOS_Casc_df1_Quantized:
    """ This class implements the direct-form 1 implementation of an IIR filter made from cascaded SOS.
        The filter's coefficients and data is quantized and the structure is closely related to the VHDL-implementation.
        This allows to use the inputs and outputs from this filter as testbench-stimuli.

        Cannot use numpy-arrays/data types here, as its integers are limited to 64 bits, which could be exceeded!
    """

    def __init__(self, sosd, gd, gain_input, sosgain_en, finalgain_en, w_dat_input, w_sect_dat, w_dat_output, w_coef,
                 w_frac):
        """ Constructor:
            :param sosd: list of lists containing the quantized SOS coefficients [b0, b1, b2, 1, a1, a2]
            :param gs: list of the quantized section/filter gains.
                        Length depends on sosgain_en and finalgain_en (See IIR_Config.py)
            :param gain_input: Filter input gain; must be power of two.
            :param sosgain_en: If set to true, the section-gains are enabled
            :param finalgain_en: If set to true, the filter's final output gain is enabled
            :param w_dat_input: Data width (bits) of (signed) input data
            :param w_sect_dat: Data width (bits) of filter-internal / SOS input/output data vectors
            :param w_dat_ouput: Data width (bits) of filter output data
            :param w_coef: Data width (bits) of coefficients
            :param w_frac: Data width (bits) of fraction of coefficients
        """
        # Create local variables and convert to appropriate type:
        self.__sos = [[int(y) for y in x] for x in sosd]  # Note that python's ints have infinite precision (python 3)
        self.__g = [int(x) for x in gd]
        self.__sosgain_en = bool(sosgain_en)
        self.__finalgain_en = bool(finalgain_en)
        self.__w_coef = int(w_coef)
        self.__w_frac = int(w_frac)
        self.__w_sect_dat = int(w_sect_dat)
        self.__w_dat_input = int(w_dat_input)
        self.__w_dat_output = int(w_dat_output)
        self.__gain_input = int(gain_input)
        # Nr. of filter sections:
        self.__sos_np = np.array(sosd) # Numpy-array just for auxiliary functions (e.g., shape etc.)
        self.__numsec, _ = self.__sos_np.shape
        # Section registers of direct-form 1. Each section has a set of registers. They store the filter's state.
        # Note that it is convert back to python 3 lists, in order to be able to use the high-precision integers.
        self.__b0Reg = np.zeros(self.__numsec)
        self.__b0Reg.tolist()
        self.__b0Reg = [int(x) for x in self.__b0Reg]  # Use python's integers, not numpy's.
        self.__b1Reg = np.zeros(self.__numsec)
        self.__b1Reg.tolist()
        self.__b1Reg = [int(x) for x in self.__b1Reg]  # Use python's integers, not numpy's.
        self.__b2Reg = np.zeros(self.__numsec)
        self.__b2Reg.tolist()
        self.__b2Reg = [int(x) for x in self.__b2Reg]  # Use python's integers, not numpy's.
        self.__a1Reg = np.zeros(self.__numsec)
        self.__a1Reg.tolist()
        self.__a1Reg = [int(x) for x in self.__a1Reg]  # Use python's integers, not numpy's.
        self.__a2Reg = np.zeros(self.__numsec)
        self.__a2Reg.tolist()
        self.__a2Reg = [int(x) for x in self.__a2Reg]  # Use python's integers, not numpy's.
        self.__satReg = np.zeros(self.__numsec)
        self.__satReg.tolist()
        self.__satReg = [int(x) for x in self.__satReg]  # Use python's integers, not numpy's.
        self.__outReg = np.zeros(self.__numsec)
        self.__outReg.tolist()
        self.__outReg = [int(x) for x in self.__outReg]  # Use python's integers, not numpy's.
        # Width of the multiplier output, to check for overflow
        self.__w_mul = self.__w_coef + self.__w_sect_dat

        # Make some basic sanity-checks:
        sos_x, sos_y = self.__sos_np.shape
        self.__g_np = np.array(gd)
        len_g, = self.__g_np.shape
        # Check correct format of coefficients. They have to be supplied as list of [b0, b1, b2, 1, a1, a2].
        if sos_y != 6:
            raise RuntimeError('Number of SOS coefficients per section is incorrect.'
                               'Deliver section-coefficients as list of: [b0, b1, b2, 1, a1, a2]')

        # Check, if the length of the output-gain vector corresponds to the selection settings (sosgain_en, finalgain_en)
        if sosgain_en is False and finalgain_en is False:
            if len_g != 0:
                raise RuntimeError('If both SOSGAIN_EN and FINALGAIN_EN are False, G must be an empty list.')
        elif sosgain_en is True and finalgain_en is False:
            if len_g != sos_x:
                raise RuntimeError('If SOSGAIN_EN is True and FINALGAIN_EN is False, G must be as long as  '
                                   'there are sections.')
        elif sosgain_en is True and finalgain_en is True:
            if len_g != sos_x + 1:
                raise RuntimeError('If SOSGAIN_EN and FINALGAIN_EN are True, G must be as long as  '
                                   'the number of sections plus 1. The last entry in G is the final output gain.')
        elif sosgain_en is False and finalgain_en is True:
            if len_g != 1:
                raise RuntimeError('If SOSGAIN_EN is False and FINALGAIN_EN is True, G must be a list of length 1,  '
                                   'containing the filter output gain.')

    def update_filter(self, input):
        """ Updates the filter with a new input and returns the new output sample. """

        # Check if the input is integer, since this class operates with quantized values / integer algebra
        if isinstance(input, int) is False:
            raise RuntimeError('Filter input must be an integer. Use Python 3 integers for arbitrary precision.')

        if self.__checkbounds_signed(input, self.__w_dat_input) is False:
            raise RuntimeError('Filter input is outside of allowed range (signed bit-vector of length w_dat_input)')

        # Apply the filter's input gain:
        input = input * self.__gain_input

        # Check if the multiplied value is within the range of the section-width.
        # This should be the case; the hardware bit-extends the left-shifted input vector and
        # this should fit into w_sect_dat
        if self.__checkbounds_signed(input, self.__w_sect_dat) is False:
            raise RuntimeError(
                'Left-shifted filter input is outside of allowed range (signed bit-vector of length w_sect_dat)')

        # Update the filter's states and calculate the new output:
        # First, iterate through all sections and calculate the direct-form 1's new states.
        # Then, apply the output gains, if applicable.
        for i in range(0, self.__numsec):
            # Shift each section register:
            self.__a2Reg[i] = self.__a1Reg[i]
            self.__a1Reg[i] = self.__satReg[i]
            self.__b2Reg[i] = self.__b1Reg[i]
            self.__b1Reg[i] = self.__b0Reg[i]
            # The input register is either the filter input or the output of the previous section,
            # depending on the current section:
            if i == 0:
                self.__b0Reg[i] = input
            else:
                self.__b0Reg[i] = self.__outReg[i - 1]

            # Calculate the new state of the current direct-form 1 section.
            # These variables hold the MulAccs:
            accu = int(0)

            # b0Reg * b0:
            mul = self.__mul_check_ovf(self.__b0Reg[i], self.__sos[i][0], self.__w_mul)
            # Accumulate:
            accu = self.__add_wrap(accu, mul, self.__w_mul)
            # b1Reg * b1:
            mul = self.__mul_check_ovf(self.__b1Reg[i], self.__sos[i][1], self.__w_mul)
            # Accumulate:
            accu = self.__add_wrap(accu, mul, self.__w_mul)
            # b2Reg * b2:
            mul = self.__mul_check_ovf(self.__b2Reg[i], self.__sos[i][2], self.__w_mul)
            # Accumulate:
            accu = self.__add_wrap(accu, mul, self.__w_mul)
            # a1Reg * a1: Note: sos[i, 3] is always 1!
            mul = self.__mul_check_ovf(self.__a1Reg[i], self.__sos[i][4], self.__w_mul)
            # Accumulate: Note the sign
            accu = self.__add_wrap(accu, -1 * mul, self.__w_mul)
            # a2Reg * a2:
            mul = self.__mul_check_ovf(self.__a2Reg[i], self.__sos[i][5], self.__w_mul)
            # Accumulate: Note the sign
            accu = self.__add_wrap(accu, -1 * mul, self.__w_mul)

            # Divide by 2**W_FRAC: Make an integer division to keep the int.
            accu = accu >> self.__w_frac

            # Saturate the output to the section's data width
            accusat, satrequired = self.__saturate(accu, self.__w_sect_dat)
            if satrequired is True:
                print('Note: Section-accu: Saturation was required.')

            self.__satReg[i] = accusat  # Store the saturated output

            # Apply the section-output-gain, if necessary:
            if self.__sosgain_en is True:
                mul = self.__mul_check_ovf(self.__satReg[i], self.__g[i], self.__w_mul)
                # Divide by 2**W_FRAC.
                mul = mul >> self.__w_frac
                # Saturate to the section's data width:
                outgainsat, satrequired = self.__saturate(mul, self.__w_sect_dat)
                if satrequired is True:
                    print('Note: Section-outgain: Saturation was required.')
                # Store the section result:
                self.__outReg[i] = outgainsat
            else:
                # No output gain of sos-section: store the saturated accu:
                self.__outReg[i] = self.__satReg[i]

        # Iterated through all the sections. Now: apply the filter's output gain, if enabled:
        if self.__finalgain_en is True:
            # Take the last section's output and apply the final filter gain. Note the indices of outReg and g;
            # The very last entry in g holds the final output gain.
            finalout = self.__mul_check_ovf(self.__outReg[self.__numsec - 1], self.__g[-1], self.__w_mul)
            # Divide by 2**W_FRAC:
            finalout = np.right_shift(finalout, self.__w_frac)
        else:
            # No final gain enabled: directly output the content of the last section:
            finalout = self.__outReg[self.__numsec - 1]

        # Saturate (either the final-multiplied value or the last section's output) to the filter's output:
        finalsat, satrequired = self.__saturate(finalout, self.__w_dat_output)
        if satrequired is True:
            print('Note: Filter output: Saturation was required.')

        # Return the filter's output:
        return finalsat

    def __saturate(self, a, w_bit):
        """ This saturates a value to a signed bit-vector of length w_bit
            Returns a tuple of the saturated value and a boolean that indicates whether saturation happened or not.
        """
        # Use Python 3's integers:
        max = int(2 ** (w_bit - 1) - 1)
        min = int(-1 * 2 ** (w_bit - 1))
        if a > max:
            a = max
            sat = True
        elif a < min:
            a = min
            sat = True
        else:
            sat = False
        return a, sat

    def __mul_check_ovf(self, a, b, w_bit):
        """ This implements a multiplier that checks for overflow. 
            A runtime error is raised if the result is outside the signed range of the w_bit-vector.
        """
        # Make sure to have enough bits for even wide filters ==> use Python 3's integers!
        result = int(a) * int(b)

        if self.__checkbounds_signed(result, w_bit) is False:
            raise RuntimeError('Multiplication result out of bounds.')
        else:
            return result

    def __add_wrap(self, a, b, w_bit):
        """ This implements a wrapping adder. 
            Emulates the behavior of a signed binary adder.
            a and b are added, w_bit is the number of bits of the adder, which it will wrap around.
        """
        # Use Python 3's integers:
        max = int(2 ** (w_bit - 1) - 1)
        min = int(-1 * 2 ** (w_bit - 1))

        result = int(a) + int(b)
        # Wrap the result:
        if result > max:
            diff = result - max
            print('Note: Adder wrapped over top')  # Just a notification
            return min + (diff - 1)
        elif result < min:
            diff = min - result
            print('Note. Adder wrapped over bottom')
            return max - (diff - 1)
        return result

    def __checkbounds_signed(self, input, w_bit):
        """ This function checks if the delivered value is in the range of a signed integer of width w_bit
            Returns true if this is the case, false otherwise.
        """
        # Use Python 3's integers:
        max = int(2 ** (w_bit - 1) - 1)
        min = int(-1 * 2 ** (w_bit - 1))
        if input < min or input > max:
            return False
        else:
            return True
