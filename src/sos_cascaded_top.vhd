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
-- VHDL Naming Convention:
-- http://dz.ee.ethz.ch/en/information/hdl-help/vhdl-naming-conventions.html
--------------------------------------------------------------------------------------------------------------
--
-- VIIRF - Versatile IIR Filter
-- sos_cascaded_top.vhd
--
-- This is the filter's top-level file.
-- It applies the input gain, connects the direct-form 1 SOS/biquads in series, and applies
-- the output gain, if enabled.
--
-- Note: DatxDO is only valid when StrbxSO = '1'! The data can change
-- inbetween. 
--
-- The filter coefficients are provided to this unit via a generic coefficient
-- interface comprising the following signals. The coefficient are then stored in a
-- RAM (no vendor-specific constructs used). 
--
-- SectAddrxDI:
--      Address of the SOS/biquad for which the coefficients are provided.
--      Range: 0-NUM_SEC. Maximum allowable NUM_SEC: 255.
--      If SectAddrxDI = NUM_SEC and CoeffAddrxDI = 6, the filter's final
--      output gain is addressed. 
-- CoeffAddrxDI:
--      Address of the selected SOS coefficient. Range: 0-6. Coefficient mapping:
--      0 ==> b0
--      1 ==> b1
--      2 ==> b2
--      3 ==> a1
--      4 ==> a2
--      5 ==> g
--      6 ==> FinalGain (Can only be written if SectAddrxDI=NUM_SEC)
-- CoeffDatxDI:
--      Coefficient data of the addressed coefficient
-- CoeffValidxSI:
--      If set to high, data and addresses are valid and the coefficient data
--      is stored in an internal RAM. 
-- 
-- Generics/Configuration:
--
-- NUM_SEC:
--      Number of cascaded biquad sections to include. Max value: 255
-- W_DAT_INPUT:
--      Width of (signed) input data vector
-- GAIN_INPUT:
--      Filter input gain. Must be a power of two (left-shifts). Usable for
--      bit-extension / precision increase. If not used: set to 1.
-- W_SECT_DAT:
--      Width of SOS data path / width of SOS inputs/outputs. 
-- W_COEF:
--      Overall coefficient width (bits)
-- W_FRAC:
--      Fraction length of the coefficients (bits). Together with W_COEF, this
--      defines the Q-Notation of the quantized coefficients.
--      E.g., for Q1.15, set W_COEF=16 and W_FRAC=15.
-- SOSGAIN_EN:
--      Bool, if set to true, the output-gain stage of the SOS is generated / included
-- FINALGAIN_EN:
--      Bool, if set to true, the final filter output-gain stage is generated /
--      included
-- W_DAT_OUTPUT:
--      Width of the (signed) output data vector. Data is saturated to this
--      vector width.
-- W_DAT_INTF:
--      Width of the (std_logic_vector) address and data vectors of the generic
--      coefficient data interface.
-- USE_PIPELINE_CORE:
--      Bool, if set to true, the pipelined SOS core is used (more resource
--      usage, but higher clock rate possible). If set to false, the reuse-core
--      is utilized, which uses a single multiplier and a state-machine to
--      control the data flow.

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
use IEEE.math_real.all;


entity sos_cascaded_top is
  generic(
    NUM_SEC           : integer := 27;
    W_DAT_INPUT       : integer := 16;
    GAIN_INPUT        : integer := 4;
    W_SECT_DAT        : integer := 23;
    W_COEF            : integer := 18;
    W_FRAC            : integer := 16;
    SOSGAIN_EN        : boolean := false;
    FINALGAIN_EN      : boolean := true;
    W_DAT_OUTPUT      : integer := 32;
    W_DAT_INTF        : integer := 32;
    USE_PIPELINE_CORE : boolean := true
    );
  port (
    ClkxCI        : in  std_logic;      -- Clock input
    RstxRI        : in  std_logic;      -- Reset input, active high
    -- Filter Data:
    DatxDI        : in  signed(W_DAT_INPUT-1 downto 0);  -- Input data
    StrbxSI       : in  std_logic;      -- Input strobe
    -- Output data - Only valid with StrbxSO!
    DatxDO        : out signed(W_DAT_OUTPUT-1 downto 0);
    -- Output strobe - Indicates validity of DatxDO                                 
    StrbxSO       : out std_logic;
    -- Coefficient Configuration Interface: Description: See above
    SectAddrxDI   : in  std_logic_vector(W_DAT_INTF-1 downto 0);
    CoeffAddrxDI  : in  std_logic_vector(W_DAT_INTF-1 downto 0);
    CoeffDatxDI   : in  std_logic_vector(W_DAT_INTF-1 downto 0);
    CoeffValidxSI : in  std_logic
    );
end sos_cascaded_top;

architecture Behavioral of sos_cascaded_top is

-----------------------------------------------------------------------------
-- Components
-----------------------------------------------------------------------------

  -- This core is the pipelined core optimized for high clock rates.
  -- This comes at the cost of heavier multiplier usage. 
  component sos_core_df1 is
    generic (
      W_DAT      : integer;
      W_COEF     : integer;
      W_FRAC     : integer;
      SOSGAIN_EN : boolean);
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
      gxDI    : in  signed(W_COEF-1 downto 0));
  end component sos_core_df1;


  -- This core uses a state-machine arount a single multiplier.
  -- Due to this additional logic around the multiplier, its clock frequency is
  -- more limited than the fully pipelined implementation of the core. 
  component sos_core_df1_reuse is
    generic (
      W_DAT      : integer;
      W_COEF     : integer;
      W_FRAC     : integer;
      SOSGAIN_EN : boolean);
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
      gxDI    : in  signed(W_COEF-1 downto 0));
  end component sos_core_df1_reuse;

-----------------------------------------------------------------------------
-- Signal Declarations
-----------------------------------------------------------------------------

  -------------
  -- Constants:
  -------------
  -- Constants for the filter coefficient interface:
  -- Width of addresses for sections and coefficients of coefficient
  -- config interface. This is hardcoded.
  constant W_SECT_ADDR   : integer := 8;  -- Allows 255 sections
  constant W_COEFF_ADDR  : integer := 3;  -- A range from 0 - 6 is required
  -- Number of coefficients per section: 6 for SOS. (b0, b1, b2, a1, a2, g).
  -- This is fixed by the direct-form 1 implementation.
  constant NUM_COEF_SECT : integer := 6;

  -- Number of left-shifts required for filter input gain / bit extension.
  -- Note: GAIN_INPUT has to be a power of two, otherwise, the gain will not be
  -- what's expected...
  constant NUM_INPUT_LSHIFT  : integer                                       := integer(ceil(log2(real(GAIN_INPUT))));
  -- Vector to hold the expanded bits:
  constant INPUT_EXPAND_VECT : std_logic_vector(NUM_INPUT_LSHIFT-1 downto 0) := (others => '0');

  -- Values for output saturation. Two's complement min and max values, defined
  -- using bit-masks instead of integers, as W_DAT_OUTPUT can be > 32 bit.
  constant SAT_OUT_POS : signed(W_DAT_OUTPUT-1 downto 0) := (W_DAT_OUTPUT-1 => '0', others => '1');
  constant SAT_OUT_NEG : signed(W_DAT_OUTPUT-1 downto 0) := (W_DAT_OUTPUT-1 => '1', others => '0');

  -----------
  -- Signals:
  -----------

  -- Coefficient interface registers and signals:
  signal SectAddrxDN, SectAddrxDP     : std_logic_vector(W_DAT_INTF-1 downto 0) := (others => '0');
  signal CoeffAddrxDN, CoeffAddrxDP   : std_logic_vector(W_DAT_INTF-1 downto 0) := (others => '0');
  signal CoeffDatxDN, CoeffDatxDP     : std_logic_vector(W_DAT_INTF-1 downto 0) := (others => '0');
  signal CoeffValidxSN, CoeffValidxSP : std_logic                               := '0';
  signal SectAddrxS                   : unsigned(W_SECT_ADDR-1 downto 0)        := (others => '0');
  signal CoeffAddrxS                  : unsigned(W_COEFF_ADDR-1 downto 0)       := (others => '0');

  -- Coefficient registers/RAM: This stores the filter coefficients of each section.
  -- Each section has its own set of registers:
  type COEFREG_TYPE is array (NUM_COEF_SECT-1 downto 0) of signed(W_COEF-1 downto 0);
  type SECTREGS_TYPE is array (NUM_SEC-1 downto 0) of COEFREG_TYPE;
  -- RAM to store the 6 coefficients of each section.
  signal CoeffRamxDN, CoeffRamxDP   : SECTREGS_TYPE             := (others => (others => (others => '0')));
  -- Final output gain register. This exists only once per filter.
  signal FinalGainxDN, FinalGainxDP : signed(W_COEF-1 downto 0) := (others => '0');

  -- Connection wires between the series-connected second-order sections.
  -- One less than NUM_SEC required:
  type STRBWIRE_TYPE is array (NUM_SEC-2 downto 0) of std_logic;
  type DATWIRE_TYPE is array (NUM_SEC-2 downto 0) of signed(W_SECT_DAT-1 downto 0);
  signal StrbWiresxS : STRBWIRE_TYPE := (others => '0');
  signal DatWiresxS  : DATWIRE_TYPE  := (others => (others => '0'));

  -- Various registers/signals:
  signal RstxRN, RstxRP                 : std_logic                      := '0';
  signal InputDatxDN, InputDatxDP       : signed(W_DAT_INPUT-1 downto 0) := (others => '0');
  signal InputStrbxSN, InputStrbxSP     : std_logic                      := '0';
  -- Register for holding the left-shifted input signal. Bit-Expanded to
  -- W_SECT_DAT. This is input into the first SOS.
  signal InputDatLSxDN, InputDatLSxDP   : signed(W_SECT_DAT-1 downto 0)  := (others => '0');
  signal InputStrbLSxSN, InputStrbLSxSP : std_logic                      := '0';
  -- Outputs from the last SOS. We don't register this since there is directly
  -- a register at the output of the SOS-core.
  signal SOSDatxS                       : signed(W_SECT_DAT-1 downto 0)  := (others => '0');
  signal SOSStrbxS                      : std_logic                      := '0';


-----------------------------------------------------------------------------
begin  -- Behavioral
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Wiring
-----------------------------------------------------------------------------
  -- Registering of the coefficient interface for route-timing relaxation
  CoeffAddrxDN  <= CoeffAddrxDI;
  SectAddrxDN   <= SectAddrxDI;
  CoeffDatxDN   <= CoeffDatxDI;
  CoeffValidxSN <= CoeffValidxSI;

  -- For internal use: Crop the address vectors to a shorter length.
  -- Potentially reduces routing delays.
  CoeffAddrxS <= unsigned(CoeffAddrxDP(W_COEFF_ADDR-1 downto 0));
  SectAddrxS  <= unsigned(SectAddrxDP(W_SECT_ADDR-1 downto 0));

  -- Local reset, for better timing if filter is (area-wise) large.
  -- This also resets the cascaded sections.
  RstxRN <= RstxRI;

  -- Data input register: For timing relaxation
  InputDatxDN  <= DatxDI;
  InputStrbxSN <= StrbxSI;

  -- Apply the Input-Gain and expand the bit-width for the following biquad sections:
  InputDatLSxDN  <= resize(signed(std_logic_vector(InputDatxDP) & INPUT_EXPAND_VECT), W_SECT_DAT);
  -- Also carry the strobe forward:
  InputStrbLSxSN <= InputStrbxSP;

-----------------------------------------------------------------------------
-- Processes
-----------------------------------------------------------------------------

  -- Reads the coefficients from the coefficient data interface
  -- and stores them to the coefficient RAM.
  CoeffWrite : process(CoeffAddrxS, CoeffDatxDP, CoeffRamxDP, CoeffValidxSP,
                       FinalGainxDP, SectAddrxS)
  begin
    -- Default Assignments:
    CoeffRamxDN  <= CoeffRamxDP;
    FinalGainxDN <= FinalGainxDP;
    -- Only write when valid = 1:
    if CoeffValidxSP = '1' then
      -- Write the regular section's coefficients:
      if SectAddrxS <= NUM_SEC-1 and CoeffAddrxS <= 5 then
        CoeffRamxDN(to_integer(SectAddrxS))(to_integer(CoeffAddrxS)) <= resize(signed(CoeffDatxDP), W_COEF);
      -- Write the final output gain of the filter:
      elsif SectAddrxS = NUM_SEC and CoeffAddrxS = 6 then
        FinalGainxDN <= resize(signed(CoeffDatxDP), W_COEF);
      end if;
    end if;
  end process CoeffWrite;

  -- Only create / generate the final output multiplier if it's actually enabled.
  -- This saves resources and latency.
  -- The signals defined in this block are not visible outside this
  -- generate-statement, hence there is a clocking-process here, too.
  -- The required latency for the strobe-signal is 4
  GenerateFinalMul : if FINALGAIN_EN = true generate
    constant W_MUL                              : integer                                       := W_COEF + W_SECT_DAT;
    constant W_RS                               : integer                                       := W_MUL - W_FRAC;
    signal gRegxDN, gRegxDP                     : signed(W_MUL-1 downto 0)                      := (others => '0');
    signal ShiftGainOutxDN, ShiftGainOutxDP     : signed(W_MUL-1 downto 0)                      := (others => '0');
    signal ShiftGainOutRSxDN, ShiftGainOutRSxDP : signed(W_RS-1 downto 0)                       := (others => '0');
    signal OutMulxDN, OutMulxDP                 : signed(W_DAT_OUTPUT-1 downto 0)               := (others => '0');
    -- Number of latency-cycles required for the strobe-output, if the final
    -- gain is used:
    constant NUM_CYC_DEL_STRB                   : integer                                       := 4;
    -- Strobe-shift register: The input strobe is shifted "through" this unit:
    signal StrbShiftxSN, StrbShiftxSP           : std_logic_vector(NUM_CYC_DEL_STRB-1 downto 0) := (others => '0');
  begin

    -- This process manages the section's output gain and saturation:
    FinalGain : process(FinalGainxDP, SOSDatxS, ShiftGainOutxDP, gRegxDP,
                        shiftGainOutRSxDP)
    begin
      -- Apply the final gain:
      gRegxDN           <= SOSDatxS * FinalGainxDP;
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
        OutMulxDN <= resize(shiftGainOutRSxDP, W_DAT_OUTPUT);
      end if;
    end process FinalGain;

    -- The core's data output is the one where the final output gain has been applied to:
    DatxDO <= OutMulxDP;

    -- Delay the input strobe according to the latency of the core:
    StrobeShiftOutGain : process(SOSStrbxS,
                                 StrbShiftxSP(NUM_CYC_DEL_STRB-2 downto 0))
    begin
      StrbShiftxSN(0)                           <= SOSStrbxS;
      StrbShiftxSN(NUM_CYC_DEL_STRB-1 downto 1) <= StrbShiftxSP(NUM_CYC_DEL_STRB-2 downto 0);
    end process StrobeShiftOutGain;

    -- Strobe output:
    StrbxSO <= StrbShiftxSP(NUM_CYC_DEL_STRB-1);

    --The flip-flops needed if the output gain is used:
    FF_FinalMul : process (ClkxCI)
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
    end process FF_FinalMul;

  end generate;


  -- If the final filter output gain is not used: simply saturate the
  -- SOS-output to the output width of the filter.  
  -- The required latency for the strobe-signal is 1
  GenerateNoFinalMul : if FINALGAIN_EN = false generate

    signal OutSatxDN, OutSatxDP       : signed(W_DAT_OUTPUT-1 downto 0)               := (others => '0');
    -- Number of latency-cycles required for the strobe-output, if the final
    -- gain is used:
    constant NUM_CYC_DEL_STRB         : integer                                       := 1;
    -- Strobe-shift register: The input strobe is shifted "through" this unit:
    signal StrbShiftxSN, StrbShiftxSP : std_logic_vector(NUM_CYC_DEL_STRB-1 downto 0) := (others => '0');
  begin

    -- Saturation:
    OutSat : process(SOSDatxS)
    begin
      if SOSDatxS > SAT_OUT_POS then
        OutSatxDN <= SAT_OUT_POS;
      elsif SOSDatxS < SAT_OUT_NEG then
        OutSatxDN <= SAT_OUT_NEG;
      else
        OutSatxDN <= resize(SOSDatxS, W_DAT_OUTPUT);
      end if;
    end process OutSat;

    -- The core's data output is now saturated:
    DatxDO <= OutSatxDP;

    -- Delay the input strobe according to the latency of the core:
    StrobeShiftOutGain : process(SOSStrbxS,
                                 StrbShiftxSP(NUM_CYC_DEL_STRB-2 downto 0))
    begin
      StrbShiftxSN(0)                           <= SOSStrbxS;
      StrbShiftxSN(NUM_CYC_DEL_STRB-1 downto 1) <= StrbShiftxSP(NUM_CYC_DEL_STRB-2 downto 0);
    end process StrobeShiftOutGain;

    -- Strobe output:
    StrbxSO <= StrbShiftxSP(NUM_CYC_DEL_STRB-1);

    --The flip-flops needed if there is no final output gain:
    FF_NoFinalMul : process (ClkxCI)
    begin
      if ClkxCI'event and ClkxCI = '1' then
        if RstxRI = '1' then
          OutSatxDP    <= (others => '0');
          StrbShiftxSP <= (others => '0');
        else
          OutSatxDP    <= OutSatxDN;
          StrbShiftxSP <= StrbShiftxSN;
        end if;
      end if;
    end process FF_NoFinalMul;

  end generate;


  -- The local reset flip-flop:
  RstFF : process (ClkxCI)
  begin
    if ClkxCI'event and ClkxCI = '1' then
      RstxRP <= RstxRN;
    end if;
  end process RstFF;


  -- The flip-flops:
  FF : process (ClkxCI)
  begin
    if ClkxCI'event and ClkxCI = '1' then
      if RstxRP = '1' then
        SectAddrxDP    <= (others => '0');
        CoeffAddrxDP   <= (others => '0');
        CoeffDatxDP    <= (others => '0');
        CoeffValidxSP  <= '0';
        CoeffRamxDP    <= (others => (others => (others => '0')));
        FinalGainxDP   <= (others => '0');
        InputDatxDP    <= (others => '0');
        InputStrbxSP   <= '0';
        InputDatLSxDP  <= (others => '0');
        InputStrbLSxSP <= '0';
      else
        SectAddrxDP    <= SectAddrxDN;
        CoeffAddrxDP   <= CoeffAddrxDN;
        CoeffDatxDP    <= CoeffDatxDN;
        CoeffValidxSP  <= CoeffValidxSN;
        CoeffRamxDP    <= CoeffRamxDN;
        FinalGainxDP   <= FinalGainxDN;
        InputDatxDP    <= InputDatxDN;
        InputStrbxSP   <= InputStrbxSN;
        InputDatLSxDP  <= InputDatLSxDN;
        InputStrbLSxSP <= InputStrbLSxSN;
      end if;
    end if;
  end process FF;


-----------------------------------------------------------------------------
-- Instances
-----------------------------------------------------------------------------

  -- In the following, the SOS instances are cascaded.
  -- Both is done either for the pipelined or the reuse-core.
  -- Furthermore, if there is only one, two or more sections, the wiring is
  -- slightly different, as there are or are no middle or output sections.

  -- The pipelined cores are used:
  UsePipelineCore : if USE_PIPELINE_CORE = true generate

    -- Only a single SOS; Connect it directly to input/output
    SingleSec : if NUM_SEC = 1 generate
      SingleSOS : sos_core_df1
        generic map (
          W_DAT      => W_SECT_DAT,
          W_COEF     => W_COEF,
          W_FRAC     => W_FRAC,
          SOSGAIN_EN => SOSGAIN_EN)
        port map (
          ClkxCI  => ClkxCI,
          RstxRI  => RstxRP,
          DatxDI  => InputDatLSxDP,
          StrbxSI => InputStrbLSxSP,
          DatxDO  => SOSDatxS,
          StrbxSO => SOSStrbxS,
          b0xDI   => CoeffRamxDP(0)(0),
          b1xDI   => CoeffRamxDP(0)(1),
          b2xDI   => CoeffRamxDP(0)(2),
          a1xDI   => CoeffRamxDP(0)(3),
          a2xDI   => CoeffRamxDP(0)(4),
          gxDI    => CoeffRamxDP(0)(5));
    end generate SingleSec;

    -- Two cascaded SOS; Connected in series
    DualSec : if NUM_SEC = 2 generate
      InputSOS : sos_core_df1
        generic map (
          W_DAT      => W_SECT_DAT,
          W_COEF     => W_COEF,
          W_FRAC     => W_FRAC,
          SOSGAIN_EN => SOSGAIN_EN)
        port map (
          ClkxCI  => ClkxCI,
          RstxRI  => RstxRP,
          DatxDI  => InputDatLSxDP,
          StrbxSI => InputStrbLSxSP,
          DatxDO  => DatWiresxS(0),
          StrbxSO => StrbWiresxS(0),
          b0xDI   => CoeffRamxDP(0)(0),
          b1xDI   => CoeffRamxDP(0)(1),
          b2xDI   => CoeffRamxDP(0)(2),
          a1xDI   => CoeffRamxDP(0)(3),
          a2xDI   => CoeffRamxDP(0)(4),
          gxDI    => CoeffRamxDP(0)(5));

      OutSOS : sos_core_df1
        generic map (
          W_DAT      => W_SECT_DAT,
          W_COEF     => W_COEF,
          W_FRAC     => W_FRAC,
          SOSGAIN_EN => SOSGAIN_EN)
        port map (
          ClkxCI  => ClkxCI,
          RstxRI  => RstxRP,
          DatxDI  => DatWiresxS(0),
          StrbxSI => StrbWiresxS(0),
          DatxDO  => SOSDatxS,
          StrbxSO => SOSStrbxS,
          b0xDI   => CoeffRamxDP(1)(0),
          b1xDI   => CoeffRamxDP(1)(1),
          b2xDI   => CoeffRamxDP(1)(2),
          a1xDI   => CoeffRamxDP(1)(3),
          a2xDI   => CoeffRamxDP(1)(4),
          gxDI    => CoeffRamxDP(1)(5));
    end generate DualSec;

    -- More than 2 SOS sections; there is an input, middle and output section.
    -- Wired into a cascade
    MoreSec : if NUM_SEC > 2 generate
      CascadeSections : for I in 0 to NUM_SEC-1 generate
        InputSection : if I = 0 generate
          InputSOS : sos_core_df1
            generic map (
              W_DAT      => W_SECT_DAT,
              W_COEF     => W_COEF,
              W_FRAC     => W_FRAC,
              SOSGAIN_EN => SOSGAIN_EN)
            port map (
              ClkxCI  => ClkxCI,
              RstxRI  => RstxRP,
              DatxDI  => InputDatLSxDP,
              StrbxSI => InputStrbLSxSP,
              DatxDO  => DatWiresxS(I),
              StrbxSO => StrbWiresxS(I),
              b0xDI   => CoeffRamxDP(I)(0),
              b1xDI   => CoeffRamxDP(I)(1),
              b2xDI   => CoeffRamxDP(I)(2),
              a1xDI   => CoeffRamxDP(I)(3),
              a2xDI   => CoeffRamxDP(I)(4),
              gxDI    => CoeffRamxDP(I)(5));
        end generate InputSection;

        MiddleSections : if I > 0 and I < NUM_SEC-1 generate
          MidSOS : sos_core_df1
            generic map (
              W_DAT      => W_SECT_DAT,
              W_COEF     => W_COEF,
              W_FRAC     => W_FRAC,
              SOSGAIN_EN => SOSGAIN_EN)
            port map (
              ClkxCI  => ClkxCI,
              RstxRI  => RstxRP,
              DatxDI  => DatWiresxS(I-1),
              StrbxSI => StrbWiresxS(I-1),
              DatxDO  => DatWiresxS(I),
              StrbxSO => StrbWiresxS(I),
              b0xDI   => CoeffRamxDP(I)(0),
              b1xDI   => CoeffRamxDP(I)(1),
              b2xDI   => CoeffRamxDP(I)(2),
              a1xDI   => CoeffRamxDP(I)(3),
              a2xDI   => CoeffRamxDP(I)(4),
              gxDI    => CoeffRamxDP(I)(5));
        end generate MiddleSections;

        OutputSection : if I = NUM_SEC-1 generate
          OutSOS : sos_core_df1
            generic map (
              W_DAT      => W_SECT_DAT,
              W_COEF     => W_COEF,
              W_FRAC     => W_FRAC,
              SOSGAIN_EN => SOSGAIN_EN)
            port map (
              ClkxCI  => ClkxCI,
              RstxRI  => RstxRP,
              DatxDI  => DatWiresxS(I-1),
              StrbxSI => StrbWiresxS(I-1),
              DatxDO  => SOSDatxS,
              StrbxSO => SOSStrbxS,
              b0xDI   => CoeffRamxDP(I)(0),
              b1xDI   => CoeffRamxDP(I)(1),
              b2xDI   => CoeffRamxDP(I)(2),
              a1xDI   => CoeffRamxDP(I)(3),
              a2xDI   => CoeffRamxDP(I)(4),
              gxDI    => CoeffRamxDP(I)(5));
        end generate OutputSection;
      end generate CascadeSections;
    end generate MoreSec;
  end generate UsePipelineCore;

  -- The non-pipelined, FSM-controlled cores are used:
  NoUsePipelineCore : if USE_PIPELINE_CORE = false generate

    -- Only a single SOS; Connect it directly to input/output
    SingleSec : if NUM_SEC = 1 generate
      SingleSOS : sos_core_df1_reuse
        generic map (
          W_DAT      => W_SECT_DAT,
          W_COEF     => W_COEF,
          W_FRAC     => W_FRAC,
          SOSGAIN_EN => SOSGAIN_EN)
        port map (
          ClkxCI  => ClkxCI,
          RstxRI  => RstxRP,
          DatxDI  => InputDatLSxDP,
          StrbxSI => InputStrbLSxSP,
          DatxDO  => SOSDatxS,
          StrbxSO => SOSStrbxS,
          b0xDI   => CoeffRamxDP(0)(0),
          b1xDI   => CoeffRamxDP(0)(1),
          b2xDI   => CoeffRamxDP(0)(2),
          a1xDI   => CoeffRamxDP(0)(3),
          a2xDI   => CoeffRamxDP(0)(4),
          gxDI    => CoeffRamxDP(0)(5));
    end generate SingleSec;

    -- Two cascaded SOS; Connected in series
    DualSec : if NUM_SEC = 2 generate
      InputSOS : sos_core_df1_reuse
        generic map (
          W_DAT      => W_SECT_DAT,
          W_COEF     => W_COEF,
          W_FRAC     => W_FRAC,
          SOSGAIN_EN => SOSGAIN_EN)
        port map (
          ClkxCI  => ClkxCI,
          RstxRI  => RstxRP,
          DatxDI  => InputDatLSxDP,
          StrbxSI => InputStrbLSxSP,
          DatxDO  => DatWiresxS(0),
          StrbxSO => StrbWiresxS(0),
          b0xDI   => CoeffRamxDP(0)(0),
          b1xDI   => CoeffRamxDP(0)(1),
          b2xDI   => CoeffRamxDP(0)(2),
          a1xDI   => CoeffRamxDP(0)(3),
          a2xDI   => CoeffRamxDP(0)(4),
          gxDI    => CoeffRamxDP(0)(5));

      OutSOS : sos_core_df1_reuse
        generic map (
          W_DAT      => W_SECT_DAT,
          W_COEF     => W_COEF,
          W_FRAC     => W_FRAC,
          SOSGAIN_EN => SOSGAIN_EN)
        port map (
          ClkxCI  => ClkxCI,
          RstxRI  => RstxRP,
          DatxDI  => DatWiresxS(0),
          StrbxSI => StrbWiresxS(0),
          DatxDO  => SOSDatxS,
          StrbxSO => SOSStrbxS,
          b0xDI   => CoeffRamxDP(1)(0),
          b1xDI   => CoeffRamxDP(1)(1),
          b2xDI   => CoeffRamxDP(1)(2),
          a1xDI   => CoeffRamxDP(1)(3),
          a2xDI   => CoeffRamxDP(1)(4),
          gxDI    => CoeffRamxDP(1)(5));
    end generate DualSec;

    -- More than 2 SOS sections; there is an input, middle and output section.
    -- Wired into a cascade
    MoreSec : if NUM_SEC > 2 generate
      CascadeSections : for I in 0 to NUM_SEC-1 generate
        InputSection : if I = 0 generate
          InputSOS : sos_core_df1_reuse
            generic map (
              W_DAT      => W_SECT_DAT,
              W_COEF     => W_COEF,
              W_FRAC     => W_FRAC,
              SOSGAIN_EN => SOSGAIN_EN)
            port map (
              ClkxCI  => ClkxCI,
              RstxRI  => RstxRP,
              DatxDI  => InputDatLSxDP,
              StrbxSI => InputStrbLSxSP,
              DatxDO  => DatWiresxS(I),
              StrbxSO => StrbWiresxS(I),
              b0xDI   => CoeffRamxDP(I)(0),
              b1xDI   => CoeffRamxDP(I)(1),
              b2xDI   => CoeffRamxDP(I)(2),
              a1xDI   => CoeffRamxDP(I)(3),
              a2xDI   => CoeffRamxDP(I)(4),
              gxDI    => CoeffRamxDP(I)(5));
        end generate InputSection;

        MiddleSections : if I > 0 and I < NUM_SEC-1 generate
          MidSOS : sos_core_df1_reuse
            generic map (
              W_DAT      => W_SECT_DAT,
              W_COEF     => W_COEF,
              W_FRAC     => W_FRAC,
              SOSGAIN_EN => SOSGAIN_EN)
            port map (
              ClkxCI  => ClkxCI,
              RstxRI  => RstxRP,
              DatxDI  => DatWiresxS(I-1),
              StrbxSI => StrbWiresxS(I-1),
              DatxDO  => DatWiresxS(I),
              StrbxSO => StrbWiresxS(I),
              b0xDI   => CoeffRamxDP(I)(0),
              b1xDI   => CoeffRamxDP(I)(1),
              b2xDI   => CoeffRamxDP(I)(2),
              a1xDI   => CoeffRamxDP(I)(3),
              a2xDI   => CoeffRamxDP(I)(4),
              gxDI    => CoeffRamxDP(I)(5));
        end generate MiddleSections;

        OutputSection : if I = NUM_SEC-1 generate
          OutSOS : sos_core_df1_reuse
            generic map (
              W_DAT      => W_SECT_DAT,
              W_COEF     => W_COEF,
              W_FRAC     => W_FRAC,
              SOSGAIN_EN => SOSGAIN_EN)
            port map (
              ClkxCI  => ClkxCI,
              RstxRI  => RstxRP,
              DatxDI  => DatWiresxS(I-1),
              StrbxSI => StrbWiresxS(I-1),
              DatxDO  => SOSDatxS,
              StrbxSO => SOSStrbxS,
              b0xDI   => CoeffRamxDP(I)(0),
              b1xDI   => CoeffRamxDP(I)(1),
              b2xDI   => CoeffRamxDP(I)(2),
              a1xDI   => CoeffRamxDP(I)(3),
              a2xDI   => CoeffRamxDP(I)(4),
              gxDI    => CoeffRamxDP(I)(5));
        end generate OutputSection;
      end generate CascadeSections;
    end generate MoreSec;
  end generate NoUsePipelineCore;

end Behavioral;
