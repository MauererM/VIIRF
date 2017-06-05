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
-- sos_core_df1_reuse.vhd
--
-- This is the implementation of a direct-form 1 SOS/biquad.
-- It implements the following difference equation:
-- y[n] = g*(b0*x[n] + b1*x[n-1] + b2*x[n-2] - a1*y[n-1] - a2*y[n-2])
--
-- The core utilizes a single multiplier (it is reused), and a state-machine
-- takes care that the multiplier gets supplied with the correct data. This
-- saves resources, but due to the increased logic complexity to feed the
-- multiplier with data, the clock rate is more limited than in the pipelined
-- version of this core (sos_core_df1.vhd).
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

entity sos_core_df1_reuse is
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
end sos_core_df1_reuse;

architecture Behavioral of sos_core_df1_reuse is

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

  -- Values for output saturation. Two's complement min and max values, defined
  -- using bit-masks instead of integers, as W_DAT can be > 32 bit.
  constant SAT_OUT_POS : signed(W_DAT-1 downto 0) := (W_DAT-1 => '0', others => '1');
  constant SAT_OUT_NEG : signed(W_DAT-1 downto 0) := (W_DAT-1 => '1', others => '0');

  -----------
  -- Signals:
  -----------
  type STATES is (IDLE, MULACC1, MULACC2, MULACC3, MULACC4, MULACC5, MULACC6, DIVFRAC, SATOUT, GAINOUT, GAINOUT_DIVFRAC, SATGAINOUT);
  signal StatexSN, StatexSP : STATES := IDLE;

  -- Main registers for the direct-form 1 implementation:
  signal b0RegxDN, b0RegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal b1RegxDN, b1RegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal b2RegxDN, b2RegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal a1RegxDN, a1RegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal a2RegxDN, a2RegxDP : signed(W_DAT-1 downto 0) := (others => '0');

  -- The Multiply-Accumulator and right-shifts:
  signal MulxDN, MulxDP                   : signed(W_MUL-1 downto 0) := (others => '0');
  signal AccuxDN, AccuxDP                 : signed(W_MUL-1 downto 0) := (others => '0');
  signal ShiftOutxDN, ShiftOutxDP         : signed(W_MUL-1 downto 0) := (others => '0');
  signal ShiftOutGainxDN, ShiftOutGainxDP : signed(W_MUL-1 downto 0) := (others => '0');

  -- Output and output-gain register/signals:
  signal SatRegxDN, SatRegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal OutRegxDN, OutRegxDP : signed(W_DAT-1 downto 0) := (others => '0');
  signal StrbxSN, StrbxSP     : std_logic                := '0';

  -- Coefficient buffer registers: For reducing routing delay. These registers
  -- are ''static'', i.e., don't change during the core's operation.
  signal b0xDN, b0xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal b1xDN, b1xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal b2xDN, b2xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal a1xDN, a1xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal a2xDN, a2xDP : signed(W_COEF-1 downto 0) := (others => '0');
  signal gxDN, gxDP   : signed(W_COEF-1 downto 0) := (others => '0');

-----------------------------------------------------------------------------
begin  -- Behavioral
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Wiring
-----------------------------------------------------------------------------
  -- Buffer registers for timing relaxation (routing delay):
  b0xDN   <= b0xDI;
  b1xDN   <= b1xDI;
  b2xDN   <= b2xDI;
  a1xDN   <= a1xDI;
  a2xDN   <= a2xDI;
  gxDN    <= gxDI;
  -- Outputs:
  DatxDO  <= OutRegxDP;
  StrbxSO <= StrbxSP;

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
  -- In this implementation, it is implemented as a FSM; the
  -- multiply-accumulator is reused. This saves pyhsical multipliers. 
  DataCalc : process(AccuxDP, MulxDP, OutRegxDP, SatRegxDP, ShiftOutGainxDP,
                     ShiftOutxDP, StatexSP, StrbxSI, a1RegxDP, a1xDP, a2RegxDP,
                     a2xDP, b0RegxDP, b0xDP, b1RegxDP, b1xDP, b2RegxDP, b2xDP,
                     gxDP)
  begin
    -- Default Assignments:
    StatexSN        <= StatexSP;
    StrbxSN         <= '0';
    MulxDN          <= MulxDP;
    AccuxDN         <= AccuxDP;
    ShiftOutxDN     <= ShiftOutxDP;
    ShiftOutGainxDN <= ShiftOutGainxDP;
    SatRegxDN       <= SatRegxDP;
    OutRegxDN       <= OutRegxDP;

    case StatexSP is
      when IDLE =>
        AccuxDN <= (others => '0');
        if StrbxSI = '1' then
          StatexSN <= MULACC1;
        end if;

      when MULACC1 =>
        MulxDN   <= b0RegxDP * b0xDP;
        StatexSN <= MULACC2;

      when MULACC2 =>
        AccuxDN  <= AccuxDP + MulxDP;
        MulxDN   <= b1RegxDP * b1xDP;
        StatexSN <= MULACC3;

      when MULACC3 =>
        AccuxDN  <= AccuxDP + MulxDP;
        MulxDN   <= b2RegxDP * b2xDP;
        StatexSN <= MULACC4;

      when MULACC4 =>
        AccuxDN  <= AccuxDP + MulxDP;
        MulxDN   <= a1RegxDP * a1xDP;
        StatexSN <= MULACC5;

      when MULACC5 =>
        AccuxDN  <= AccuxDP - MulxDP;   -- Negative sign
        MulxDN   <= a2RegxDP * a2xDP;
        StatexSN <= MULACC6;

      when MULACC6 =>
        AccuxDN  <= AccuxDP - MulxDP;   -- Negative sign
        StatexSN <= DIVFRAC;

      when DIVFRAC =>
        -- Output divider: Get rid of the gain due to the integer algebra (2^W_FRAC):
        ShiftOutxDN <= shift_right(AccuxDP, W_FRAC);
        StatexSN    <= SATOUT;

      when SATOUT =>
        -- Check for output overflow and saturate, if necessary:
        -- Set the output-register already here too, in case SOSGAIN_EN is false
        if ShiftOutxDP > SAT_OUT_POS then
          SatRegxDN <= SAT_OUT_POS;
          OutRegxDN <= SAT_OUT_POS;
        elsif ShiftOutxDP < SAT_OUT_NEG then
          SatRegxDN <= SAT_OUT_NEG;
          OutRegxDN <= SAT_OUT_NEG;
        else
          SatRegxDN <= resize(ShiftOutxDP, W_DAT);
          OutRegxDN <= resize(ShiftOutxDP, W_DAT);
        end if;
        -- Check if the output gain has to be applied:
        if SOSGAIN_EN = true then
          StatexSN <= GAINOUT;
        else
          -- Done, OutReg can be output / strobed valid: 
          StatexSN <= IDLE;
          StrbxSN  <= '1';
        end if;

      when GAINOUT =>
        -- Reuse the MulAcc from above also here. If timing is a bit more
        -- critical, this could be replaced by an additional multiplier.
        MulxDN   <= SatRegxDP * gxDP;
        StatexSN <= GAINOUT_DIVFRAC;

      when GAINOUT_DIVFRAC =>
        ShiftOutGainxDN <= shift_right(MulxDP, W_FRAC);
        StatexSN        <= SATGAINOUT;

      when SATGAINOUT =>
        -- Check for output overflow and saturate, if necessary:
        if ShiftOutGainxDP > SAT_OUT_POS then
          OutRegxDN <= SAT_OUT_POS;
        elsif ShiftOutGainxDP < SAT_OUT_NEG then
          OutRegxDN <= SAT_OUT_NEG;
        else
          OutRegxDN <= resize(ShiftOutGainxDP, W_DAT);
        end if;
        -- Done, OutReg can be output / strobed valid: 
        StatexSN <= IDLE;
        StrbxSN  <= '1';

      when others =>
        StatexSN <= IDLE;
    end case;

  end process DataCalc;

  -- The flip-flops for the direct-form 1 implementation:
  FF : process (ClkxCI)
  begin
    if ClkxCI'event and ClkxCI = '1' then
      if RstxRI = '1' then
        StatexSP        <= IDLE;
        b0RegxDP        <= (others => '0');
        b1RegxDP        <= (others => '0');
        b2RegxDP        <= (others => '0');
        a1RegxDP        <= (others => '0');
        a2RegxDP        <= (others => '0');
        MulxDP          <= (others => '0');
        AccuxDP         <= (others => '0');
        ShiftOutxDP     <= (others => '0');
        ShiftOutGainxDP <= (others => '0');
        SatRegxDP       <= (others => '0');
        OutRegxDP       <= (others => '0');
        StrbxSP         <= '0';
        b0xDP           <= (others => '0');
        b1xDP           <= (others => '0');
        b2xDP           <= (others => '0');
        a1xDP           <= (others => '0');
        a2xDP           <= (others => '0');
        gxDP            <= (others => '0');
      else
        StatexSP        <= StatexSN;
        b0RegxDP        <= b0RegxDN;
        b1RegxDP        <= b1RegxDN;
        b2RegxDP        <= b2RegxDN;
        a1RegxDP        <= a1RegxDN;
        a2RegxDP        <= a2RegxDN;
        MulxDP          <= MulxDN;
        AccuxDP         <= AccuxDN;
        ShiftOutxDP     <= ShiftOutxDN;
        ShiftOutGainxDP <= ShiftOutGainxDN;
        SatRegxDP       <= SatRegxDN;
        OutRegxDP       <= OutRegxDN;
        StrbxSP         <= StrbxSN;
        b0xDP           <= b0xDN;
        b1xDP           <= b1xDN;
        b2xDP           <= b2xDN;
        a1xDP           <= a1xDN;
        a2xDP           <= a2xDN;
        gxDP            <= gxDN;
      end if;
    end if;
  end process FF;


-----------------------------------------------------------------------------
-- Instances
-----------------------------------------------------------------------------
-- No additional instances.


end Behavioral;
