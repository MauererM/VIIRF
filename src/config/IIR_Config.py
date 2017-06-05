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

    This stand-alone script is used to configure the cascaded IIR filter hardware.
    It checks the provided data for errors and generates multiple files used for configuring the filter's generics
    and the testbenches.It also calculates the filter's step-response (both floating-point and quantized filter)

    Python: 3.5.2
    NOTE: The behavior of e.g., integers, longs or integer divisions differs from python 2 and 3!
    Python 3 is preferred/required due to the infinite integer-precision provided by python 3.
    Do NOT use NUMPY-datatypes for filter calculations. They are limited to 64 bit and this is quickly not enough
    for high-precision / low-noise filters.

"""
import numpy as np
import math
import datetime
import os
import matplotlib.pyplot as plt
from IIR_SOS_Filt_df1 import SOS_Casc_df1_Float, SOS_Casc_df1_Quantized


def main():
    """ Implementation of the filter-configuration script.
    """

    """ USER CONFIGURATION: """

    """ SOS filter coefficients of direct-form 1 filter implementation 
        Deliver as list of: [b0, b1, b2, 1, a1, a2]
    """
    """ This example-filter is an over-the-top lowpass filter to illustrate the numerical stability 
        of the cascaded biquads.
        Designed with Matlab's fdatool.
        Chebyshev Type II, fs = 8MHz, fpass = 100kHz, fstop = 103kHz, Astop=100dB, Apass=1dB
        27 sections, order = 53
    """
    SOS = [
        [0.995479543133467093, -1.984442343028223199, 0.995479543133467093, 1.000000000000000000, -1.992735837405045007,
         0.998943837906379417],
        [0.990160401678707558, -1.973793179424183863, 0.990160401678707558, 1.000000000000000000, -1.990574581081460126,
         0.996817448646951321],
        [0.989994808108325630, -1.973370399921241303, 0.989994808108325630, 1.000000000000000000, -1.988322781675001094,
         0.994642785115447126],
        [0.991233302433756958, -1.975696584512673271, 0.991233302433756958, 1.000000000000000000, -1.985941378398403678,
         0.992382744424644914],
        [0.991659186917251634, -1.976348896290843538, 0.991659186917251634, 1.000000000000000000, -1.983386802441603702,
         0.989996937324842396],
        [0.991088443582708867, -1.974955022041059438, 0.991088443582708867, 1.000000000000000000, -1.980609068243228243,
         0.987439951339094235],
        [0.989791457625747717, -1.972046311400692975, 0.989791457625747717, 1.000000000000000000, -1.977549417505724660,
         0.984659251857910700],
        [0.987954582977658147, -1.967983929921569342, 0.987954582977658147, 1.000000000000000000, -1.974137317922536994,
         0.981592546952532841],
        [0.985631182144554474, -1.962860596839200111, 0.985631182144554474, 1.000000000000000000, -1.970286545701848580,
         0.978164380415427415],
        [0.982774011953296389, -1.956564178822880251, 0.982774011953296389, 1.000000000000000000, -1.965889972689622844,
         0.974281625675177509],
        [0.979253170496530601, -1.948812091805058877, 0.979253170496530601, 1.000000000000000000, -1.960812519064707216,
         0.969827417018972904],
        [0.974849525369247516, -1.939136275833626355, 0.974849525369247516, 1.000000000000000000, -1.954881497685421854,
         0.964652853916498465],
        [0.969223223534541378, -1.926817757162398781, 0.969223223534541378, 1.000000000000000000, -1.947873233632636314,
         0.958565521324582837],
        [0.961850833163988006, -1.910757730499921481, 0.961850833163988006, 1.000000000000000000, -1.939494351783139114,
         0.951313448987238353],
        [0.951915030351768388, -1.889252974781179262, 0.951915030351768388, 1.000000000000000000, -1.929355449638937792,
         0.942562554513742157],
        [0.938119438257709160, -1.859621313051272651, 0.938119438257709160, 1.000000000000000000, -1.916934024503464062,
         0.931864889025346255],
        [0.918395732636982709, -1.817613163905800633, 0.918395732636982709, 1.000000000000000000, -1.901522707459767370,
         0.918614305102019579],
        [0.889505718582964788, -1.756619076035403682, 0.889505718582964788, 1.000000000000000000, -1.882158880178752547,
         0.901986187040051623],
        [0.846725210100101044, -1.667053994501234815, 0.846725210100101044, 1.000000000000000000, -1.857535164039942233,
         0.880860806020991594],
        [0.784291731375497903, -1.537281285666471931, 0.784291731375497903, 1.000000000000000000, -1.825904322530537804,
         0.853741893377616523],
        [0.697754992932913987, -1.358310063925080913, 0.697754992932913987, 1.000000000000000000, -1.785035725009338492,
         0.818719360505246518],
        [0.587659395799238982, -1.131033533640340982, 0.587659395799238982, 1.000000000000000000, -1.732396994259293388,
         0.773624816388282732],
        [0.459692699491257184, -0.866373131097798344, 0.459692699491257184, 1.000000000000000000, -1.665995235127884611,
         0.716751802598463050],
        [0.320384043209753555, -0.577024918342495030, 0.320384043209753555, 1.000000000000000000, -1.586704538299201772,
         0.648848540201372170],
        [0.180337233564054289, -0.285068622336177968, 0.180337233564054289, 1.000000000000000000, -1.502750870671594852,
         0.576957777166854702],
        [0.067223579650870141, -0.048886921409928924, 0.067223579650870141, 1.000000000000000000, -1.433955574106285269,
         0.518049962948276210],
        [0.149602974506854058, 0.149602974506854058, 0.000000000000000000, 1.000000000000000000, -0.703314433339987333,
         0.000000000000000000]]

    """ Filter section gain-values and overall output gain.
        This array can have different dimensions/lengths:   
        If SOSGAIN_EN is true: It must be at least as long as the number of sections, as each section 
            requires an individual gain.
        If FINALGAIN_EN is true: It must be at least one element long, the final output gain of the filter.
        If neither of these two parameters is true: It must be an empty list.
        If both parameters are true: The list must be the number of sections plus 1 (section-gains + output gain)
    """
    G = [0.158364443548321743]

    """ Defines if section-output-gains in hardware will be enabled.
        The length of G must correspond with this setting (see above). 
    """
    SOSGAIN_EN = False

    """ Defines if a final filter output gain will be used.
        The length of G must correspond with this setting (see above). 
    """
    FINALGAIN_EN = True

    """ Number of bits of the unfiltered input data vector.
        The filter operates on signed vectors.
    """
    W_DAT_INPUT = 16

    """ Total number of bits of the filter coefficients.
        The coefficients are signed vectors. 
        See W_FRAC below for explanation of Q-notation.
    """
    W_COEF = 18

    """ Number of bits used for the fraction of the quantized coefficients. Must be <= W_COEF.
        Integer arithmetic: Q-Notation: The difference between W_COE and W_FRAC are the integer bits.
        E.g., for Q1.15, it would require W_COEF = 16 and W_FRAC = 15.
    """
    W_FRAC = 16

    """ Filter-input gain: Must be a power of two.
        The filter gain can be used to extend the number of bits of the filtered data, 
        in order to increase the precision/filter resolution.
        If not used/desired: Set to 1. 
        W_SECT_DAT (filter-internal vector width) will depend on this value.
    """
    GAIN_INPUT = 4

    """ Number of bits of (signed) filter output vector
        Filtered result will be saturated to the range of this (signed) vector. 
        If filter overshoots (to full-scale inputs) should be covered correctly, use at least 1 bit more 
        than at filter input (W_DAT_INPUT).
    """
    W_DAT_OUTPUT = 32

    """ This script can only take the required bit-extension caused by the final gain into account. 
        If the SOS-gains are used, it could be necessary to manually extend the filter-internal vector width. 
        This can be done with this parameter. It directly affects W_SECT_DAT. 
        Adjust if the sections saturate or the filter does not achieve the desired response due to vector length limits.
        Set to 0 if not used/required.
    """
    NUM_BITS_SECT_DAT_EXT_MANUAL = 0

    """ Simulation-specific user-configuration. This does not affect the filter parameters.
    """
    STEP_AMPLITUDE = 1.0  # Amplitude of the step for the step-response. Range: 0-1
    NUM_STEP_SIM = 800  # Number of filter iterations for the simulation of the step-response

    """ File-Names and directories for Data Storage.
        This script generates various text-files containing filter configurations and testbench stimuli. 
    """
    FILENAME_METADATA = 'FilterMetaData.txt'  # Stores information for the VHDL generic header
    DIRNAME_STIMULIDATA = 'FilterStimuliData'  # Directory where testbench-related data is stored.

    """ END OF USER INPUT: Implementation follows. """

    # Convert to floating-point; to homogenize the input
    SOS = [[float(y) for y in x] for x in SOS]  # SOS is a list of lists.
    G = [float(x) for x in G]
    W_DAT_INPUT = int(W_DAT_INPUT)
    W_COEF = int(W_COEF)
    W_FRAC = int(W_FRAC)
    # GAIN_INPUT = int(GAIN_INPUT) Don't do this - check for exact power of two and integer-ness later.
    W_DAT_OUTPUT = int(W_DAT_OUTPUT)

    # Check the viability of the user-supplied data. This function may raise errors and create console output.
    check_user_inputdata(SOS, G, SOSGAIN_EN, FINALGAIN_EN, W_COEF, W_FRAC, GAIN_INPUT)

    # Calculate the required internal filter vector length (W_SECT_DAT):
    w_sect_dat = get_internal_bit_width(W_DAT_INPUT, G, GAIN_INPUT, W_DAT_OUTPUT, FINALGAIN_EN)
    w_sect_dat = w_sect_dat + NUM_BITS_SECT_DAT_EXT_MANUAL
    s = 'Filter-internal bit-width (W_SECT_DAT): ' + repr(w_sect_dat)
    print(s)

    # Quantize the coefficients:
    SOSd, Gd = quantize_filter_coefs(SOS, G, W_FRAC)

    # Write the required VHDL-generics into a file for storage:
    write_filter_metadata(FILENAME_METADATA, SOS, W_DAT_INPUT, W_DAT_OUTPUT, GAIN_INPUT, w_sect_dat, W_COEF, W_FRAC,
                          FINALGAIN_EN, SOSGAIN_EN)

    # Write the coefficients into different files for the testbench:
    # First, create the directory for all testbench-related data, if necessary:
    if not os.path.exists(DIRNAME_STIMULIDATA):
        os.makedirs(DIRNAME_STIMULIDATA)
    write_filter_coe_testbench(SOSd, Gd, SOSGAIN_EN, FINALGAIN_EN, DIRNAME_STIMULIDATA)

    # Create a floating-point step-input vector. A vector would not be needed (it's all the same value),
    # but this allows an easy change to other inputs if so desired (e.g., noise).
    floatstepin = np.ones(NUM_STEP_SIM, dtype=float) * STEP_AMPLITUDE

    # Simulate the filter's step response with the floating-point implementation:
    filt_float_inst = SOS_Casc_df1_Float(SOS, G, GAIN_INPUT, SOSGAIN_EN, FINALGAIN_EN)  # Create the filter instance
    filt_float_response = []
    for i in range(0, NUM_STEP_SIM):
        filt_float_response.append(filt_float_inst.update_filter(floatstepin[i]))

    # Create the quantized step-input vector.
    max_in = int(2 ** (W_DAT_INPUT - 1) - 1)  # Maximum (signed) filter input value
    quantstepin = np.round(np.ones(NUM_STEP_SIM) * max_in * STEP_AMPLITUDE)
    quantstepin = [int(x) for x in quantstepin]  # Convert to python integer

    # Simulate the quantized filter's step response with the VHDL-reference model:
    filt_quant_inst = SOS_Casc_df1_Quantized(SOSd, Gd, GAIN_INPUT, SOSGAIN_EN, FINALGAIN_EN, W_DAT_INPUT, w_sect_dat,
                                             W_DAT_OUTPUT, W_COEF, W_FRAC)
    filt_quant_response = []
    for i in range(0, NUM_STEP_SIM):
        filt_quant_response.append(filt_quant_inst.update_filter(quantstepin[i]))

    # Convert the quantized output to float and rescale back such that the plots can be compared
    filt_quant_response_float = [float(x) / max_in / STEP_AMPLITUDE for x in filt_quant_response]

    # Write the digital filter input (stimuli) and reference output into two files for testbench-use.
    filename_stims = os.path.join(DIRNAME_STIMULIDATA, 'InputDat_Stimuli.txt')
    write_list_file(filename_stims, quantstepin)
    filename_stims = os.path.join(DIRNAME_STIMULIDATA, 'OutputDat_GoldenReference.txt')
    write_list_file(filename_stims, filt_quant_response)

    print('Done. Displaying the plot.')

    # Plot the filter's step-response:
    fig = plt.figure()
    ax = fig.add_subplot(111)
    x = np.arange(1, NUM_STEP_SIM + 1, 1)
    plt.step(x, filt_float_response, label='Floating-Point', marker='o')
    plt.step(x, filt_quant_response_float, label='Quantized (Rescaled)', marker='*')
    ax.set_xlabel('Sample Nr.')
    ax.set_ylabel('Filter Response')
    ax.set_title('Filter Step Responses')
    plt.legend()
    plt.grid()
    plt.show()  # This will block until the plotted window is closed.


def write_list_file(filename, dat):
    """ Writes the content of the list "dat" into a text-file.
        Existing files will be cleared / overwritten.
    """
    clear_txt_file(filename)
    [write_string_append(filename, repr(x)) for x in dat]


def write_filter_coe_testbench(sosd, gd, sosgain_en, finalgain_en, dirpath):
    """ Writes the coefficients in multiple files needed for the filter testbench
        dirpath is a string pointing to the desired directory where the files will be created in.
    """
    sosd_np = np.array(sosd)
    numsec, _ = sosd_np.shape
    filename_b0 = os.path.join(dirpath, 'CoeffStimuli_b0.txt')
    filename_b1 = os.path.join(dirpath, 'CoeffStimuli_b1.txt')
    filename_b2 = os.path.join(dirpath, 'CoeffStimuli_b2.txt')
    filename_a1 = os.path.join(dirpath, 'CoeffStimuli_a1.txt')
    filename_a2 = os.path.join(dirpath, 'CoeffStimuli_a2.txt')
    filename_g = os.path.join(dirpath, 'CoeffStimuli_g.txt')
    filename_finalgain = os.path.join(dirpath, 'CoeffStimuli_finalgain.txt')

    clear_txt_file(filename_b0)
    clear_txt_file(filename_b1)
    clear_txt_file(filename_b2)
    clear_txt_file(filename_a1)
    clear_txt_file(filename_a2)
    # Note, these values are cleared and it will always have to be written something to the gain-files,
    # since the testbench always reads them.
    clear_txt_file(filename_g)
    clear_txt_file(filename_finalgain)

    # Note that the SOS-coeffs are given as list of: [b0, b1, b2, 1, a1, a2]
    # b0, b1, b2, a1, a2 are always required:
    b0 = sosd_np[:, 0]
    b1 = sosd_np[:, 1]
    b2 = sosd_np[:, 2]
    a1 = sosd_np[:, 4]
    a2 = sosd_np[:, 5]

    if sosgain_en is True and finalgain_en is True:
        g = gd[:-1]  # The section gains: all but the last element, which is the output gain
        outgain = gd[-1]  # Last element is the final output gain
    elif sosgain_en is True and finalgain_en is False:
        g = gd  # There is no output-gain
        outgain = 0  # Write bogus-value; the filter will not use it, but the testbench will want to read it.
    elif sosgain_en is False and finalgain_en is True:
        outgain = gd[-1]  # G contains only the output gain. Get it out of the list.
        g = np.zeros(numsec, dtype=int)  # Write zero; to allow testbench-read
    elif sosgain_en is False and finalgain_en is False:
        outgain = 0  # Write zero; to allow testbench-read
        g = np.zeros(numsec, dtype=int)  # Write zero; to allow testbench-read

    # Write the coefficients of the sections into their files:
    [write_string_append(filename_b0, repr(x)) for x in b0]
    [write_string_append(filename_b1, repr(x)) for x in b1]
    [write_string_append(filename_b2, repr(x)) for x in b2]
    [write_string_append(filename_a1, repr(x)) for x in a1]
    [write_string_append(filename_a2, repr(x)) for x in a2]
    [write_string_append(filename_g, repr(x)) for x in g]
    write_string_append(filename_finalgain, repr(outgain))


def write_filter_metadata(filename, sos, w_dat_input, w_dat_output, gain_input, w_sect_dat, w_coef, w_frac,
                          finalgain_en, sosgain_en):
    """ Writes the filter's meta-data (for VHDL generic) into a text-file for storage."""
    clear_txt_file(filename)
    t = datetime.datetime.now().strftime("%d.%m.%Y - %H:%M:%S")
    sos_np = np.array(sos)
    numsec, _ = sos_np.shape  # Nr. of filter sections
    write_string_append(filename, t + '\n')
    write_string_append(filename, 'generic(')
    write_string_append(filename, 'NUM_SEC : integer := ' + repr(numsec) + ';')
    write_string_append(filename, 'W_DAT_INPUT : integer := ' + repr(w_dat_input) + ';')
    write_string_append(filename, 'GAIN_INPUT : integer := ' + repr(gain_input) + ';')
    write_string_append(filename, 'W_SECT_DAT : integer := ' + repr(w_sect_dat) + ';')
    write_string_append(filename, 'W_COEF : integer := ' + repr(w_coef) + ';')
    write_string_append(filename, 'W_FRAC : integer := ' + repr(w_frac) + ';')
    if sosgain_en is True:
        write_string_append(filename, 'SOSGAIN_EN : boolean := true;')
    else:
        write_string_append(filename, 'SOSGAIN_EN : boolean := false;')
    if finalgain_en is True:
        write_string_append(filename, 'FINALGAIN_EN : boolean := true;')
    else:
        write_string_append(filename, 'FINALGAIN_EN : boolean := false;')
    write_string_append(filename, 'W_DAT_OUTPUT : integer := ' + repr(w_dat_output) + ';')
    write_string_append(filename, 'W_DAT_INTF : integer := SET_SOMETHING;')
    write_string_append(filename, 'USE_PIPELINE_CORE : boolean := SET_SOMETHING')  # No semicolon - last entry
    write_string_append(filename, ');')


def clear_txt_file(filename):
    """ Clears the content of a text-file.
        If the file does not exist, a new empty file with this name is created.
    """
    with open(filename, "w") as file:
        pass


def write_string_append(filename, string):
    """ Writes a string to a file. 
        If file does not exist, it will create it. 
        If file exists, content will be overwritten.
        newline is done automatically.
    """
    with open(filename, "a") as file:
        print(string, file=file)


def quantize_filter_coefs(sos, g, w_frac):
    """ Quantizes the filter coefficients; from floating to fixed-point
        Note: Python 3's integers are arbitrary precision.
    """
    sos_d = np.round(np.array(sos) * np.power(2.0, w_frac))
    g_d = np.round(np.array(g) * np.power(2.0, w_frac))
    sos_d = sos_d.tolist()  # Convert back to python-3 list instead of numpy-array due to better precision.
    sos_d = [[int(y) for y in x] for x in sos_d]  # Python-3 integer.
    g_d = g_d.tolist()
    g_d = [int(x) for x in g_d]
    return (sos_d, g_d)


def get_internal_bit_width(w_dat_input, g, gain_input, w_dat_output, finalgain_en):
    """ Calculates the required internal filter widths (W_SECT_DAT).
        This only works perfectly, if only finalgain_en is true. If there are SOS-gains that are smaller than one,
        it is not accounted for by this script. W_SECT_DAT then has to be manually increased, if the filter performance
        is unsatisfactory.
    """

    # Add one bit for the filter-internal bit-width to accommodate filter overshoots up to twice the input value.
    # This is relevant for (near-) full-scale inputs, that would otherwise saturate the sections prematurely.
    w_sect_dat = w_dat_input + 1

    # Add the bits required due to the filter input gain
    # Note: gain_input has to be a power of two. This script checked this earlier (see "check_user_inputdata").
    w_sect_dat += int(math.ceil(math.log(gain_input, 2)))

    if finalgain_en is True:
        # If the final filter output gain is enabled and less than one, the internal bit-width has to be increased
        # such that precision is not lost (see below)
        # The filter output gain is the last element in G:
        gain_out = g[-1]

        """ If the final output gain is smaller than 1, it means that additional bits are needed before the final gain, 
            as the values are larger than what will be output after the output gain and our data vectors need to be able 
            to cover this extended range. (e.g., if the final output gain is 0.1 and our filter settles to 1, it have to 
            be able to store a value of 10 inside our filter.
            If the overall output gain is larger than 1, no additional bits are needed inside our filter, since it 
            "creates" the bits/information with the output gain. No check is needed if the extended vector fits into the 
            output, as it will just be saturated.
        """
        if gain_out < 1:
            """ Figure out how many bits are required to accommodate the (larger) value before the gain.
                This is done by solving the (signed-value) maximum of the two (pre/after gain) vectors for the number of 
                bits required; like so: 1 / gain_out = 2^(x-1)-1  ==> Solve for x, which is the number of additional 
                bits required due to the output gain. Solution: x = ln((2(gain_out+1))/gain_out)/ln(2) 
                ==> use ceil of this value.
            """
            numbits_outgain_addtl = int(math.ceil(math.log((2.0 * (gain_out + 1)) / gain_out) / math.log(2.0)))
            w_sect_dat += numbits_outgain_addtl

    return w_sect_dat


def check_user_inputdata(sos, g, sosgain_en, finalgain_en, w_coef, w_frac, gain_input):
    """ This function checks the user-supplied filter configuration for errors or inconsistencies.
        This makes sure the hardware/filter is not wrongly configured.
        The function may raise runtime errors and/or produces console outputs. 
    """
    sos_np = np.array(sos)  # Convert to numpy-array to get the shape (and further functions, see below)
    sos_x, sos_y = sos_np.shape
    g_np = np.array(g)
    len_g, = g_np.shape
    # Check correct format of coefficients. They have to be supplied as list of [b0, b1, b2, 1, a1, a2].
    if sos_y != 6:
        raise RuntimeError('Number of SOS coefficients per section is incorrect.'
                           'Deliver section-coefficients as list of: [b0, b1, b2, 1, a1, a2]')

    # Check if coefficient a0 of sos is fixed to 1:
    # Careful with float comparison.
    for i in range(0, sos_x):
        if np.isclose(sos_np[i, 3], 1.0, rtol=1e-8, atol=1e-9, equal_nan=False) is False:
            raise RuntimeError('SOS-coefficients at index 3 of SOS-array must all be equal to 1. '
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

    # Check, if the filter is not too long. Maximum nr. of sections: 255 (hardcoded in VHDL)
    if sos_x > 255:
        raise RuntimeError('Too many filter sections desired. Max. nr. of sections is 255 (Hardcoded in VHDL).')

    s = 'Number of SOS this filter comprises: ' + repr(sos_x)
    print(s)

    # Check, if the section-gains/output gain could be disabled:
    if sosgain_en is True:
        if finalgain_en is True:
            len_iter = len_g - 1
            if np.isclose(g_np[len_g - 1], 1.0, rtol=1e-8, atol=1e-9, equal_nan=False) is True:
                print('Detected that the final filter gain is unity. FINALGAIN_EN could be set to False.')
        else:
            len_iter = len_g
        gain_notunity = 0
        for i in range(0, len_iter):
            if np.isclose(g_np[i], 1.0, rtol=1e-8, atol=1e-9, equal_nan=False) is False:
                gain_notunity = 1
        if gain_notunity < 0.5:
            print('Detected that all section-gains are unity. SOSGAIN_EN could be set to False '
                  '(Disables output-gains of section); Less hardware-resources used.')

    # Check if the delivered coefficients are within the range of the number-space covered by the
    # coefficients/fractions.
    # Q-Notation: Signed Qm.n format.
    m = int(w_coef - w_frac)  # Nr. of integer bits
    if m < 1:
        raise RuntimeError('W_COEF - W_FRAC must be >= 1. Terminating.')
    n = int(w_frac)  # Nr. of fraction bits
    coef_min = -1.0 * float((2.0 ** (m - 1)))
    coef_max = float(2.0 ** (m - 1) - 2.0 ** (-1 * n))
    # Check, if all coefficients are in range:
    for i in range(0, sos_x):
        for k in range(0, sos_y):
            if sos[i][k] > coef_max or sos[i][k] < coef_min:
                s = 'Coefficient SOS[' + repr(i) + ',' + repr(k) + '] out of range with value ' + repr(sos[i][k])
                print(s)
                s = 'Specified Q-format: Q' + repr(m) + '.' + repr(n)
                print(s)
                s = 'Allowed range: ' + repr(coef_min) + ' < Coeff. < ' + repr(coef_max)
                print(s)
                raise RuntimeError('Coefficient out of range for specified Q-format. See console-output for details')

    # Check the gain-coefficients for conformity:
    for i in range(0, len_g):
        if g[i] > coef_max or g[i] < coef_min:
            s = 'Coefficient G[' + repr(i) + '] out of range with value ' + repr(g[i])
            print(s)
            s = 'Specified Q-format: Q' + repr(m) + '.' + repr(n)
            print(s)
            s = 'Allowed range: ' + repr(coef_min) + ' < Coeff. < ' + repr(coef_max)
            print(s)
            raise RuntimeError('Coefficient out of range for specified Q-format. See console-output for details')

    # If there is only a single section, check that finalgain is used, and not sos-gain.
    # Otherwise, the section-width calculation is not correct / the filter saturates.
    if sos_x == 1:
        if sosgain_en is True and finalgain_en is False:
            raise RuntimeError('When only using one section, use the final gain and not the SOS-gain. '
                               'Otherwise, section-data widths not calculated correctly.')

    # Check, if the filter-gain is specified as a power of two and as integer:
    if isinstance(gain_input, int) is False:
        raise RuntimeError('GAIN_INPUT must be an integer (and power of two)')
    if gain_input < 1:
        raise RuntimeError('GAIN_INPUT must be >= 1')
    if ((gain_input & (gain_input - 1)) == 0) is False:
        raise RuntimeError('GAIN_INPUT must be a power of two')

    # Check the value of the filter's overall output gain:
    if finalgain_en is True:
        gain_out = g[-1]
        if gain_out > 1:
            print('NOTE: final filter output gain is > 1. This could be covered/supported by the input gain'
                  '(and maybe an output gain < 1) for increased filter precision (but higher resource utilization'
                  'due to longer filter-internal vector widths)')


if __name__ == "__main__":
    main()  # Simply execute the main script.
