--------------------------------------------------------------------------------------------------------------
-- MIT License
--  
-- Copyright (c) 2017 Mario Mauerer
--  
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--
--------------------------------------------------------------------------------------------------------------
-- 
-- VHDL Naming Convention:
-- http://dz.ee.ethz.ch/en/information/hdl-help/vhdl-naming-conventions.html
--------------------------------------------------------------------------------------------------------------
--
-- VIIRF - Versatile IIR Filter
--
-- sos_core_df1.vhd
--
-- This is the implementation of a direct-form 1 SOS/biquad.
-- It implements the following difference equation:
-- y[n] = g*(b0*x[n] + b1*x[n-1] + b2*x[n-2] - a1*y[n-1] - a2*y[n-2])
--
-- The core is pipelined; there are registers between mayor logic elements in
-- order to be able to operate this core at a higher clock rate. This comes at
-- the cost of a higher resource utilization, especially multipliers. There is
-- a core that reuses the multiplier, at the cost of a lower max.
-- clock rate (sos_core_df1_reuse.vhd)
--
-- The coefficients are provided as signed vectors to this unit.
--
-- Generics/Configuration:
--
-- W_DAT:
--      Data width (input and output) of the core
-- W_COEF:
--      Overall coefficient width.
-- W_FRAC:
--      Fraction length of the coefficients. Together with W_COEF, this defines
--      the Q-notation of the quantized coefficients.
--      E.g., for Q1.15, set W_COEF=16 and W_FRAC=15.
-- SOSGAIN_EN:
--      Boolean that indicates whether the output-gain of the section is
--      enabled/generated, or not.

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity sos_core_df1 is
  generic(
    W_DAT      : integer := 25;
    W_COEF     : integer := 18;
    W_FRAC     : integer := 16;
    SOSGAIN_EN : boolean := true
    );
  port (
    ClkxCI  : in  std_logic;
    RstxRI  : in  std_logic;
    DatxDI  : in  signed(W_DAT-1 downto 0);
    StrbxSI : in  std_logic;
    DatxDO  : out signed(W_DAT-1 downto 0);
    StrbxSO : out std_logic;
    b0xDI   : in  signed(W_COEF-1 downto 0);
    b1xDI   : in  signed(W_COEF-1 downto 0);
    b2xDI   : in  signed(W_COEF-1 downto 0);
    a1xDI   : in  signed(W_COEF-1 downto 0);
    a2xDI   : in  signed(W_COEF-1 downto 0);
    gxDI    : in  signed(W_COEF-1 downto 0)
    );
end sos_core_df1;

architecture Behavioral of sos_core_df1 is

-----------------------------------------------------------------------------
-- Components
-----------------------------------------------------------------------------
-- No additional components.

-----------------------------------------------------------------------------
-- Signal Declarations
-----------------------------------------------------------------------------
  -------------
  -- Constants:
  -------------

  -- Data width after multipliers. This is also the width of the adders:
  constant W_MUL : integer := W_COEF + W_DAT;

  -- Data width after the right-shift operation:
  constant W_RS : integer := W_MUL - W_FRAC;

  -- Values for output saturation. Two's complement min and max values, defined
  -- using bit-masks instead of integers, as W_DAT is allowed to be > 32 bit.
  constant SAT_OUT_POS : signed(W_DAT-1 downto 0) := (W_DAT-1 => '0', others => '1');
  constant SAT_OUT_NEG : signed(W_DAT-1 downto 0) := (W_DAT-1 => '1', others => '0');

  -- Number of cycles for the strobe-output:
  constant NUM_CYC_DEL_STRB : integer := 9;

  -----------
  -- Signals:
  -----------

  -- Main registers for the direct-form 1 implementation:
  signal b0RegxDN, b0RegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal b1RegxDN, b1RegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal b2RegxDN, b2RegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal a1RegxDN, a1RegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal a2RegxDN, a2RegxDP : signed(W_DAT-1 downto 0) := (others => '0');

  -- Pipelining-registers: Between adders/multipliers for better timing:
  signal b0MulxDN, b0MulxDP           : signed(W_MUL-1 downto 0) := (others => '0');
  signal b1MulxDN, b1MulxDP           : signed(W_MUL-1 downto 0) := (others => '0');
  signal b2MulxDN, b2MulxDP           : signed(W_MUL-1 downto 0) := (others => '0');
  signal a1MulxDN, a1MulxDP           : signed(W_MUL-1 downto 0) := (others => '0');
  signal a2MulxDN, a2MulxDP           : signed(W_MUL-1 downto 0) := (others => '0');
  signal Sum1xDN, Sum1xDP             : signed(W_MUL-1 downto 0) := (others => '0');
  signal Sum2xDN, Sum2xDP             : signed(W_MUL-1 downto 0) := (others => '0');
  signal Sum3xDN, Sum3xDP             : signed(W_MUL-1 downto 0) := (others => '0');
  signal Sum4xDN, Sum4xDP             : signed(W_MUL-1 downto 0) := (others => '0');
  signal ShiftOutxDN, ShiftOutxDP     : signed(W_MUL-1 downto 0) := (others => '0');
  signal ShiftOutRSxDN, ShiftOutRSxDP : signed(W_RS-1 downto 0)  := (others => '0');

  -- Coefficient buffer registers: For reducing routing delay. These registers
  -- are ''static'', i.e., don't change during the core's operation.
  signal b0xDN, b0xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal b1xDN, b1xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal b2xDN, b2xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal a1xDN, a1xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal a2xDN, a2xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal gxDN, gxDP   : signed(W_COEF-1 downto 0) := (others => '0');

  -- Output and output-gain register/signals:
  signal SatRegxDN, SatRegxDP : signed(W_DAT-1 downto 0) := (others => '0');

-----------------------------------------------------------------------------
begin  -- Behavioral
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Wiring
-----------------------------------------------------------------------------
  -- Buffer registers for timing relaxation (routing delay):
  b0xDN <= b0xDI;
  b1xDN <= b1xDI;
  b2xDN <= b2xDI;
  a1xDN <= a1xDI;
  a2xDN <= a2xDI;
  gxDN  <= gxDI;

-----------------------------------------------------------------------------
-- Processes
-----------------------------------------------------------------------------

  -- The main registers of the direct-form 1 are strobed/forwarded here:
  RegStrb : process(DatxDI, StrbxSI, a1RegxDP, a2RegxDP, b0RegxDP, b1RegxDP,
                    b2RegxDP, satRegxDP)
  begin
    -- Default assignments:
    b0RegxDN <= b0RegxDP;
    b1RegxDN <= b1RegxDP;
    b2RegxDN <= b2RegxDP;
    a1RegxDN <= a1RegxDP;
    a2RegxDN <= a2RegxDP;
    -- These registers are only updated upon a new input strobe:
    if StrbxSI = '1' then
      b0RegxDN <= DatxDI;
      b1RegxDN <= b0RegxDP;
      b2RegxDN <= b1RegxDP;
      a1RegxDN <= satRegxDP;
      a2RegxDN <= a1RegxDP;
    end if;
  end process RegStrb;

  -- This calculates the section's new states. It also saturates the output of
  -- the core. 
  DataCalc : process(ShiftOutRSxDP, Sum1xDP, Sum2xDP, Sum3xDP, Sum4xDP,
                     a1MulxDP, a1RegxDP, a1xDP, a2MulxDP, a2RegxDP, a2xDP,
                     b0MulxDP, b0RegxDP, b0xDP, b1MulxDP, b1RegxDP, b1xDP,
                     b2MulxDP, b2RegxDP, b2xDP, shiftOutxDP)
  begin
    -- MulAccs of the direct-form 1:
    b0MulxDN      <= b0RegxDP * b0xDP;
    b1MulxDN      <= b1RegxDP * b1xDP;
    b2MulxDN      <= b2RegxDP * b2xDP;
    Sum1xDN       <= b0MulxDP + b1MulxDP;
    Sum2xDN       <= Sum1xDP + b2MulxDP;
    Sum3xDN       <= Sum2xDP - a2MulxDP;
    Sum4xDN       <= Sum3xDP - a1MulxDP;
    a1MulxDN      <= a1RegxDP * a1xDP;
    a2MulxDN      <= a2RegxDP * a2xDP;
    -- Output divider: Get rid of the gain due to the integer algebra (2^W_FRAC):
    ShiftOutxDN   <= shift_right(Sum4xDP, W_FRAC);
    -- Resize to a smaller vector for less routing delay for the upcoming saturation:
    ShiftOutRSxDN <= resize(shiftOutxDP, W_RS);

    -- Check for output overflow and saturate, if necessary:
    if ShiftOutRSxDP > SAT_OUT_POS then
      SatRegxDN <= SAT_OUT_POS;
    elsif ShiftOutRSxDP < SAT_OUT_NEG then
      SatRegxDN <= SAT_OUT_NEG;
    else
      SatRegxDN <= resize(ShiftOutRSxDP, W_DAT);
    end if;
  end process DataCalc;

  -- Only create / generate the output multiplier, if it's actually enabled.
  -- This saves DSP-resources and latency.
  -- The signals defined in this block are not visible outside this
  -- generate-statement, hence there is a clocking-process here, too.
  -- The required latency for the strobe-signal is 13
  GenerateOutMul : if SOSGAIN_EN = true generate
    signal gRegxDN, gRegxDP                     : signed(W_MUL-1 downto 0)                      := (others => '0');
    signal ShiftGainOutxDN, ShiftGainOutxDP     : signed(W_MUL-1 downto 0)                      := (others => '0');
    signal ShiftGainOutRSxDN, ShiftGainOutRSxDP : signed(W_RS-1 downto 0)                       := (others => '0');
    signal OutMulxDN, OutMulxDP                 : signed(W_DAT-1 downto 0)                      := (others => '0');
    -- Number of latency-cycles required for the strobe-output, if the output
    -- gain is used:
    constant NUM_CYC_DEL_STRB                   : integer                                       := 13;
    -- Strobe-shift register: The input strobe is shifted "through" this unit:
    signal StrbShiftxSN, StrbShiftxSP           : std_logic_vector(NUM_CYC_DEL_STRB-1 downto 0) := (others => '0');
  begin

    -- This process manages the section's output gain:
    OutGain : process(SatRegxDP, ShiftGainOutxDP, gRegxDP, gxDP,
                      shiftGainOutRSxDP)
    begin
      -- Apply the output gain:
      gRegxDN           <= SatRegxDP * gxDP;
      -- Output divider: Get rid of the gain due to the integer algebra (2^W_FRAC):
      ShiftGainOutxDN   <= shift_right(gRegxDP, W_FRAC);
      -- Resize to a smaller vector for less routing delay for the upcoming saturation:
      ShiftGainOutRSxDN <= resize(ShiftGainOutxDP, W_RS);

      -- Output Saturation:
      if ShiftGainOutRSxDP > SAT_OUT_POS then
        OutMulxDN <= SAT_OUT_POS;
      elsif ShiftGainOutRSxDP < SAT_OUT_NEG then
        OutMulxDN <= SAT_OUT_NEG;
      else
        OutMulxDN <= resize(shiftGainOutRSxDP, W_DAT);
      end if;
    end process OutGain;

    -- The core's data output is the one where the output gain has been applied
    -- to:
    DatxDO <= OutMulxDP;

    -- Delay the input strobe according to the latency of the core:
    StrobeShiftOutGain : process(StrbShiftxSP(NUM_CYC_DEL_STRB-2 downto 0),
                                 StrbxSI)
    begin
      StrbShiftxSN(0)                           <= StrbxSI;
      StrbShiftxSN(NUM_CYC_DEL_STRB-1 downto 1) <= StrbShiftxSP(NUM_CYC_DEL_STRB-2 downto 0);
    end process StrobeShiftOutGain;

    -- Strobe output:
    StrbxSO <= StrbShiftxSP(NUM_CYC_DEL_STRB-1);

    --The flip-flops needed if the output gain is deployed:
    FF_OutMul : process (ClkxCI)
    begin
      if ClkxCI'event and ClkxCI = '1' then
        if RstxRI = '1' then
          gRegxDP           <= (others => '0');
          ShiftGainOutxDP   <= (others => '0');
          ShiftGainOutRSxDP <= (others => '0');
          OutMulxDP         <= (others => '0');
          StrbShiftxSP      <= (others => '0');
        else
          gRegxDP           <= gRegxDN;
          ShiftGainOutxDP   <= ShiftGainOutxDN;
          ShiftGainOutRSxDP <= ShiftGainOutRSxDN;
          OutMulxDP         <= OutMulxDN;
          StrbShiftxSP      <= StrbShiftxSn;
        end if;
      end if;
    end process FF_OutMul;

  end generate;


  -- If the individual section gains are not used: Simply take the
  -- saturated output from the direct-form 1 implementation.
  -- The required latency for the strobe-signal is 9
  GenerateNoOutMul : if SOSGAIN_EN = false generate
    -- Number of latency-cycles required for the strobe-output, if the output
    -- gain is not used:
    constant NUM_CYC_DEL_STRB         : integer                                       := 9;
    -- Strobe-shift register: The input strobe is shifted "through" this unit:
    signal StrbShiftxSN, StrbShiftxSP : std_logic_vector(NUM_CYC_DEL_STRB-1 downto 0) := (others => '0');
  begin
    -- The output from the direct-form 1 calculation:
    DatxDO <= SatRegxDP;

    -- Delay the input strobe according to the latency of the core:
    StrobeShiftNoOutGain : process(StrbShiftxSP(NUM_CYC_DEL_STRB-2 downto 0),
                                   StrbxSI)
    begin
      StrbShiftxSN(0)                           <= StrbxSI;
      StrbShiftxSN(NUM_CYC_DEL_STRB-1 downto 1) <= StrbShiftxSP(NUM_CYC_DEL_STRB-2 downto 0);
    end process StrobeShiftNoOutGain;

    -- Strobe output:
    StrbxSO <= StrbShiftxSP(NUM_CYC_DEL_STRB-1);

    --The flip-flops needed if the output gain is not used:
    FF_NoOutMul : process (ClkxCI)
    begin
      if ClkxCI'event and ClkxCI = '1' then
        if RstxRI = '1' then
          StrbShiftxSP <= (others => '0');
        else
          StrbShiftxSP <= StrbShiftxSn;
        end if;
      end if;
    end process FF_NoOutMul;
  end generate;


  -- The flip-flops for the direct-form 1 implementation:
  FF : process (ClkxCI)
  begin
    if ClkxCI'event and ClkxCI = '1' then
      if RstxRI = '1' then
        b0RegxDP      <= (others => '0');
        b1RegxDP      <= (others => '0');
        b2RegxDP      <= (others => '0');
        a1RegxDP      <= (others => '0');
        a2RegxDP      <= (others => '0');
        b0MulxDP      <= (others => '0');
        b1MulxDP      <= (others => '0');
        b2MulxDP      <= (others => '0');
        a1MulxDP      <= (others => '0');
        a2MulxDP      <= (others => '0');
        Sum1xDP       <= (others => '0');
        Sum2xDP       <= (others => '0');
        Sum3xDP       <= (others => '0');
        Sum4xDP       <= (others => '0');
        ShiftOutxDP   <= (others => '0');
        ShiftOutRSxDP <= (others => '0');
        b0xDP         <= (others => '0');
        b1xDP         <= (others => '0');
        b2xDP         <= (others => '0');
        a1xDP         <= (others => '0');
        a2xDP         <= (others => '0');
        gxDP          <= (others => '0');
        SatRegxDP     <= (others => '0');
      else
        b0RegxDP      <= b0RegxDN;
        b1RegxDP      <= b1RegxDN;
        b2RegxDP      <= b2RegxDN;
        a1RegxDP      <= a1RegxDN;
        a2RegxDP      <= a2RegxDN;
        b0MulxDP      <= b0MulxDN;
        b1MulxDP      <= b1MulxDN;
        b2MulxDP      <= b2MulxDN;
        a1MulxDP      <= a1MulxDN;
        a2MulxDP      <= a2MulxDN;
        Sum1xDP       <= Sum1xDN;
        Sum2xDP       <= Sum2xDN;
        Sum3xDP       <= Sum3xDN;
        Sum4xDP       <= Sum4xDN;
        ShiftOutxDP   <= ShiftOutxDN;
        ShiftOutRSxDP <= ShiftOutRSxDN;
        b0xDP         <= b0xDN;
        b1xDP         <= b1xDN;
        b2xDP         <= b2xDN;
        a1xDP         <= a1xDN;
        a2xDP         <= a2xDN;
        gxDP          <= gxDN;
        SatRegxDP     <= SatRegxDN;
      end if;
    end if;
  end process FF;

-----------------------------------------------------------------------------
-- Instances
-----------------------------------------------------------------------------
-- No further instances.


end Behavioral;
