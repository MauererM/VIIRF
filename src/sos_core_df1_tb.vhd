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
-- Testbench for sos_core_df1.vhd
--
-- This testbench does not test the correct implementation of the SOS, this is
-- done by the top-level testbench (sos_cascaded_top.vhd).
--
-- This only tests signals like strobes and internal routing / delays.
-- Hence, it is a very simple unit. It simply applies some coefficients/gains
-- and checks, if the data is output correctly / with the correct strobe. 

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

-------------------------------------------------------------------------------

entity sos_core_df1_tb is
end entity sos_core_df1_tb;

-------------------------------------------------------------------------------

architecture arch_tb of sos_core_df1_tb is

  -- component generics
  constant W_DAT      : integer := 25;
  constant W_COEF     : integer := 18;
  constant W_FRAC     : integer := 16;
  constant SOSGAIN_EN : boolean := false;

  -- component ports
  signal ClkxCI  : std_logic := '1';
  signal RstxRI  : std_logic;
  signal DatxDI  : signed(W_DAT-1 downto 0);
  signal StrbxSI : std_logic;
  signal DatxDO  : signed(W_DAT-1 downto 0);
  signal StrbxSO : std_logic;
  signal b0xDI   : signed(W_COEF-1 downto 0);
  signal b1xDI   : signed(W_COEF-1 downto 0);
  signal b2xDI   : signed(W_COEF-1 downto 0);
  signal a1xDI   : signed(W_COEF-1 downto 0);
  signal a2xDI   : signed(W_COEF-1 downto 0);
  signal gxDI    : signed(W_COEF-1 downto 0);

begin  -- architecture arch_tb

  -- component instantiation
  DUT : entity work.sos_core_df1
    generic map (
      W_DAT      => W_DAT,
      W_COEF     => W_COEF,
      W_FRAC     => W_FRAC,
      SOSGAIN_EN => SOSGAIN_EN)
    port map (
      ClkxCI  => ClkxCI,
      RstxRI  => RstxRI,
      DatxDI  => DatxDI,
      StrbxSI => StrbxSI,
      DatxDO  => DatxDO,
      StrbxSO => StrbxSO,
      b0xDI   => b0xDI,
      b1xDI   => b1xDI,
      b2xDI   => b2xDI,
      a1xDI   => a1xDI,
      a2xDI   => a2xDI,
      gxDI    => gxDI);

  -- clock generation
  ClkxCI <= not ClkxCI after 5 ns;

  -- waveform generation
  WaveGen_Proc : process
  begin
    RstxRI  <= '1';
    DatxDI  <= (others => '0');
    StrbxSI <= '0';
    -- Set all coeffs to 1 or 2 in the Q-notation:
    b0xDI   <= shift_left(to_signed(1, W_COEF), W_FRAC);
    b1xDI   <= shift_left(to_signed(1, W_COEF), W_FRAC);
    b2xDI   <= shift_left(to_signed(1, W_COEF), W_FRAC);
    a1xDI   <= shift_left(to_signed(1, W_COEF), W_FRAC);
    a2xDI   <= shift_left(to_signed(1, W_COEF), W_FRAC);
    gxDI    <= shift_left(to_signed(2, W_COEF), W_FRAC);
    wait for 100 ns;
    wait until rising_edge(ClkxCI);
    RstxRI  <= '0';

    for I in 1 to 20 loop
      wait for 100 ns;
      wait until rising_edge(ClkxCI);
      DatxDI  <= to_signed(I, W_DAT);
      StrbxSI <= '1';
      wait until rising_edge(ClkxCI);
      StrbxSI <= '0';
    end loop;
    wait;
  end process WaveGen_Proc;

end architecture arch_tb;

-------------------------------------------------------------------------------

configuration sos_core_df1_tb_arch_tb_cfg of sos_core_df1_tb is
  for arch_tb
  end for;
end sos_core_df1_tb_arch_tb_cfg;

-------------------------------------------------------------------------------
