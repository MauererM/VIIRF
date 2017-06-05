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
-----------------------------------------------------------------------------
--
-- VIIRF - Versatile IIR Filter
--
-- Testbench for sos_cascaded_top.vhd
--
-- This testbench tests the top-level filter unit.
--
-- Note: The text-files used to configure, feed and compare the filter might
-- require an absolute path or a proper relative path to the simulator in their
-- declaration below.
--
-- First, it configures the filter via the coefficient data interface.
-- To achieve this, 7 text-files have to be provided:
--      CoeffStimuli_b0.txt"; ==> One new line for each section's b0-coeff.
--      CoeffStimuli_b1.txt"; ==> One new line for each section's b1-coeff.
--      CoeffStimuli_b2.txt"; ==> One new line for each section's b2-coeff.
--      CoeffStimuli_a1.txt"; ==> One new line for each section's a1-coeff.
--      CoeffStimuli_a2.txt"; ==> One new line for each section's a2-coeff.
--      CoeffStimuli_g.txt"; ==> One new line for each section's g-coeff.
--      CoeffStimuli_finalgain.txt"; ==> Contains only one number, the final gain.
--
-- NOTE: These files have also be provided and filled with data even if the SOS
-- or final filter gains are disabled! The testbench reads them nonetheless.
-- The python-configuration scripts takes this into account and fills the files
-- with zeros, if the coefficients are not required.
--
-- After configuring the filter, the testbench reads input data from the file
-- InputDat_Stimuli.txt". Each new line is a new input value.
--
-- The testbench compares the filter output to values store in the file
-- OutputDat_GoldenReference.txt". Upon mismatch, an error is raised.

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library unisim;
use unisim.vcomponents.all;
use std.textio.all;
-------------------------------------------------------------------------------

entity sos_cascaded_top_tb is
end entity sos_cascaded_top_tb;

-------------------------------------------------------------------------------

architecture arch_tb of sos_cascaded_top_tb is

  -- This is the period with which new filter data gets strobed into the
  -- filter. 
  constant T_FILT_UPDATE : time := 200 ns;

  -- Component generics
  constant NUM_SEC           : integer := 27;
  constant W_DAT_INPUT       : integer := 16;
  constant GAIN_INPUT        : integer := 4;
  constant W_SECT_DAT        : integer := 23;
  constant W_COEF            : integer := 18;
  constant W_FRAC            : integer := 16;
  constant SOSGAIN_EN        : boolean := false;
  constant FINALGAIN_EN      : boolean := true;
  constant W_DAT_OUTPUT      : integer := 32;
  constant W_DAT_INTF        : integer := 32;
  constant USE_PIPELINE_CORE : boolean := true;

  -- Component ports
  signal ClkxCI        : std_logic := '0';
  signal RstxRI        : std_logic;
  signal DatxDI        : signed(W_DAT_INPUT-1 downto 0);
  signal StrbxSI       : std_logic;
  signal DatxDO        : signed(W_DAT_OUTPUT-1 downto 0);
  signal StrbxSO       : std_logic;
  signal SectAddrxDI   : std_logic_vector(W_DAT_INTF-1 downto 0);
  signal CoeffAddrxDI  : std_logic_vector(W_DAT_INTF-1 downto 0);
  signal CoeffDatxDI   : std_logic_vector(W_DAT_INTF-1 downto 0);
  signal CoeffValidxSI : std_logic;

  -- Testbench-Internal: This signals when the filter has been configured and
  -- is ready for input data.
  signal CoeffWriteDone : std_logic := '0';

begin  -- architecture arch_tb

  -- Component instantiation
  DUT : entity work.sos_cascaded_top
    generic map (
      NUM_SEC           => NUM_SEC,
      W_DAT_INPUT       => W_DAT_INPUT,
      GAIN_INPUT        => GAIN_INPUT,
      W_SECT_DAT        => W_SECT_DAT,
      W_COEF            => W_COEF,
      W_FRAC            => W_FRAC,
      SOSGAIN_EN        => SOSGAIN_EN,
      FINALGAIN_EN      => FINALGAIN_EN,
      W_DAT_OUTPUT      => W_DAT_OUTPUT,
      W_DAT_INTF        => W_DAT_INTF,
      USE_PIPELINE_CORE => USE_PIPELINE_CORE)
    port map (
      ClkxCI        => ClkxCI,
      RstxRI        => RstxRI,
      DatxDI        => DatxDI,
      StrbxSI       => StrbxSI,
      DatxDO        => DatxDO,
      StrbxSO       => StrbxSO,
      SectAddrxDI   => SectAddrxDI,
      CoeffAddrxDI  => CoeffAddrxDI,
      CoeffDatxDI   => CoeffDatxDI,
      CoeffValidxSI => CoeffValidxSI);

  -- Clock generation
  ClkxCI <= not ClkxCI after 5 ns;

  --------------------------------------------------------------
  -- Reset process: This simply releases the reset after 100 ns.
  --------------------------------------------------------------
  RstProc : process
  begin
    RstxRI <= '1';
    wait for 100 ns;
    wait until rising_edge(ClkxCI);
    RstxRI <= '0';
    wait;
  end process RstProc;

  --------------------------------------------------------------------------------------------------
  -- Before data can be applied, this writes the coefficients from files into the RAM of the filter:
  --------------------------------------------------------------------------------------------------
  CoeffWriteProc : process
    -- These files serve the filter coefficients. Each coefficients has a
    -- separate file and each section's coefficient is on a new line (except
    -- finalgain, there is only one number required as there is only one final
    -- filter gain)
    -- THESE PATHS MIGHT REQUIRED TO BE ABSOLUTE OR PROPERLY RELATIVE TO THE
    -- SIMULATOR'S PATH.
    file COEFFILE_B0        : text is in "/testbench_stimuli/CoeffStimuli_b0.txt";
    file COEFFILE_B1        : text is in "/testbench_stimuli/CoeffStimuli_b1.txt";
    file COEFFILE_B2        : text is in "/testbench_stimuli/CoeffStimuli_b2.txt";
    file COEFFILE_A1        : text is in "/testbench_stimuli/CoeffStimuli_a1.txt";
    file COEFFILE_A2        : text is in "/testbench_stimuli/CoeffStimuli_a2.txt";
    file COEFFILE_G         : text is in "/testbench_stimuli/CoeffStimuli_g.txt";
    file COEFFILE_FINALGAIN : text is in "/testbench_stimuli/CoeffStimuli_finalgain.txt";

    variable fileline    : line;
    variable filedata    : real;
    variable SectAddrxS  : integer range 0 to NUM_SEC;
    variable CoeffAddrxS : integer range 0 to 6;
  begin
    wait until rising_edge(ClkxCI);
    if RstxRI = '1' then
      SectAddrxDI    <= (others => '0');
      CoeffAddrxDI   <= (others => '0');
      CoeffDatxDI    <= (others => '0');
      CoeffValidxSI  <= '0';
      CoeffWriteDone <= '0';
    else
      wait for 100 ns;
      wait until rising_edge(ClkxCI);

      -- Write the coefficients for each section using the coefficient interface:
      -- Go through all sections:
      for I in 0 to NUM_SEC-1 loop
        SectAddrxS := I;

        -- For each section, go through all 6 SOS-coefficients:
        for K in 0 to 5 loop
          CoeffAddrxS := K;

          if CoeffAddrxS = 0 then
            if (not endfile(COEFFILE_B0)) then
              readline(COEFFILE_B0, fileline);
              read(fileline, filedata);
            else
              assert false report "Reached end of coefficient-file without having provided all sections with data. Enough lines in coefficient-file?" severity failure;
            end if;
          elsif CoeffAddrxS = 1 then
            if (not endfile(COEFFILE_B1)) then
              readline(COEFFILE_B1, fileline);
              read(fileline, filedata);
            else
              assert false report "Reached end of coefficient-file without having provided all sections with data. Enough lines in coefficient-file?" severity failure;
            end if;
          elsif CoeffAddrxS = 2 then
            if (not endfile(COEFFILE_B2)) then
              readline(COEFFILE_B2, fileline);
              read(fileline, filedata);
            else
              assert false report "Reached end of coefficient-file without having provided all sections with data. Enough lines in coefficient-file?" severity failure;
            end if;
          elsif CoeffAddrxS = 3 then
            if (not endfile(COEFFILE_A1)) then
              readline(COEFFILE_A1, fileline);
              read(fileline, filedata);
            else
              assert false report "Reached end of coefficient-file without having provided all sections with data. Enough lines in coefficient-file?" severity failure;
            end if;
          elsif CoeffAddrxS = 4 then
            if (not endfile(COEFFILE_A2)) then
              readline(COEFFILE_A2, fileline);
              read(fileline, filedata);
            else
              assert false report "Reached end of coefficient-file without having provided all sections with data. Enough lines in coefficient-file?" severity failure;
            end if;
          elsif CoeffAddrxS = 5 then
            if (not endfile(COEFFILE_G)) then
              readline(COEFFILE_G, fileline);
              read(fileline, filedata);
            else
              assert false report "Reached end of coefficient-file without having provided all sections with data. Enough lines in coefficient-file?" severity failure;
            end if;
          end if;
          wait until rising_edge(ClkxCI);
          SectAddrxDI   <= std_logic_vector(to_unsigned(SectAddrxS, W_DAT_INTF));
          CoeffAddrxDI  <= std_logic_vector(to_unsigned(CoeffAddrxS, W_DAT_INTF));
          CoeffDatxDI   <= std_logic_vector(to_signed(natural(filedata), W_DAT_INTF));
          CoeffValidxSI <= '1';

        end loop;
      end loop;

      -- The final output-gain is transmitted by selecting NUM_SECT as SectAddr
      -- and 6 as CoeffAddr:
      CoeffValidxSI <= '1';
      wait until rising_edge(ClkxCI);
      if (not endfile(COEFFILE_FINALGAIN)) then
        readline(COEFFILE_FINALGAIN, fileline);
        read(fileline, filedata);
      else
        assert false report "Reached end of coefficient-file without having provided all sections with data. Enough lines in coefficient-file?" severity failure;
      end if;
      SectAddrxDI   <= std_logic_vector(to_unsigned(NUM_SEC, W_DAT_INTF));
      CoeffAddrxDI  <= std_logic_vector(to_unsigned(6, W_DAT_INTF));
      CoeffDatxDI   <= std_logic_vector(to_signed(natural(filedata), W_DAT_INTF));
      CoeffValidxSI <= '1';
      wait until rising_edge(ClkxCI);
      CoeffValidxSI <= '0';

      --We are done writing the coefficients, hence we simply do nothing more
      --in this process. We signal this to other processes using the flag.
      CoeffWriteDone <= '1';
      wait;
    end if;

  end process CoeffWriteProc;

  -----------------------------------------------------------------------------
  -- This reads filter input data from a file and feeds it to the filter input:
  -----------------------------------------------------------------------------
  InputDataWrite : process
    -- This file stores the filter input:
    -- THIS PATH MIGHT REQUIRED TO BE ABSOLUTE OR PROPERLY RELATIVE TO THE
    -- SIMULATOR'S PATH.
    file INPUTSTIMULI_FILE : text is in "/testbench_stimuli/InputDat_Stimuli.txt";
    variable stimuliline   : line;
    variable stimulidata   : real;
  begin
    wait until rising_edge(ClkxCI);
    if RstxRI = '1' then
      DatxDI  <= (others => '0');
      StrbxSI <= '0';
    else
      -- If the filter coefficients have been written to the filter's RAM, we
      -- can start applying data:
      if CoeffWriteDone = '1' then
        while (not endfile(INPUTSTIMULI_FILE)) loop
          readline(INPUTSTIMULI_FILE, stimuliline);
          read(stimuliline, stimulidata);
          -- We apply data periodically:
          wait for T_FILT_UPDATE;
          wait until rising_edge(ClkxCI);
          StrbxSI <= '1';
          DatxDI  <= to_signed(natural(stimulidata), W_DAT_INPUT);
          wait until rising_edge(ClkxCI);
          StrbxSI <= '0';
        end loop;
        -- If we run out of filter input data, we halt the simulation:
        assert false report "Reached end of input-stimuli file." severity failure;
      end if;
    end if;
  end process InputDataWrite;

-------------------------------------------------------------------------------
  -- This reads the output of the filter and compares it to the golden reference
  -----------------------------------------------------------------------------
  OutputDataCompare : process
    -- This file stores the filter input:
    -- THIS PATH MIGHT REQUIRED TO BE ABSOLUTE OR PROPERLY RELATIVE TO THE
    -- SIMULATOR'S PATH.
    file REF_FILE    : text is in "/testbench_stimuli/OutputDat_GoldenReference.txt";
    variable refline : line;
    variable refdata : real;
  begin
    wait until rising_edge(ClkxCI);

    if W_DAT_OUTPUT > 32 then
      assert false report "Test-bench only supports output comparisons with bit-widths less than 33 bits!" severity failure;
    end if;

    -- The filter outputs new data:
    if StrbxSO = '1' then
      if endfile(REF_FILE) then
        assert false report "Reached end of golden-reference file." severity failure;
      else
        readline(REF_FILE, refline);
        read(refline, refdata);
        -- Check if the output is correct:
        if DatxDO /= to_signed(natural(refdata), W_DAT_OUTPUT) then
          assert false report "Filter did not produce the expected output." severity failure;
        end if;
      end if;
    end if;
  end process OutputDataCompare;



end architecture arch_tb;

-------------------------------------------------------------------------------

configuration sos_cascaded_top_tb_arch_tb_cfg of sos_cascaded_top_tb is
  for arch_tb
  end for;
end sos_cascaded_top_tb_arch_tb_cfg;

-------------------------------------------------------------------------------
