----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/11/2016 03:52:26 PM
-- Design Name: 
-- Module Name: vga - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity vga is
    Port ( red       : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
           green     : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
           blue      : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
           hs_o      : out STD_LOGIC;
           vs_o      : out STD_LOGIC;
           clk_board : in STD_LOGIC := '0';
           reset     : in STD_LOGIC := '0';
           disable   : in STD_LOGIC_VECTOR (2 downto 0) := "000");
end vga;

architecture Behavioral of vga is

component vga_clk_gen
port (
  clk_in1           : in     std_logic;
  clk_out1          : out    std_logic
 );
end component;


signal clk, hs, vs  : STD_LOGIC := '0';
signal hsc          : unsigned(10 downto 0) := (others => '0');
signal vsc          : unsigned(10 downto 0) := (others => '0');
signal tmp          : STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal tmp_v        : STD_LOGIC_VECTOR(5 downto 0) := (others => '0');
--signal enable_out   : STD_LOGIC := '0';
begin

--enable_out_proc: process(clk, reset)
--begin
--   if (reset = '1') then
--       enable_out <= '0';
--   elsif (clk'event and clk='1') then
--       if(hsc = ) then
--           enable_out <= '1';
--       elsif(hsc = 640+3 or vsc = 480-1) then
--           enable_out <= '0';
--       else
--           enable_out <= enable_out;
--       end if;
--   end if;
--end process;

tmp_v <= std_logic_vector(vsc(5 downto 0)) XOR std_logic_vector(hsc(5 downto 0));
tmp   <= tmp_v & tmp_v;

pattern: process(clk)
begin
    if (clk'event and clk='1') then

        --if( enable_out = '1' ) then
        if( hsc < 1024 and vsc < 768) then
            if (disable(0) = '1' ) then
                red   <= (others => '0');
            else
                red   <= tmp(11 downto 8);
            end if;
            if (disable(1) = '1') then
                green  <= (others => '0');
            else
                green  <= tmp(7 downto 4);
            end if;
            if (disable(2) = '1') then
                blue <= (others => '0');
            else
                blue <= tmp(3 downto 0);
            end if;
        else
            red   <= (others => '0');
            green <= (others => '0');
            blue  <= (others => '0');
        end if;
    end if;
end process;

vga_clk_mapper: vga_clk_gen
port map ( 
   clk_in1 => clk_board, 
   clk_out1 => clk              
);

kiskacsa: process(hs, vs)
begin
   hs_o <= hs;
   vs_o <= vs;
end process;

counter_HS: process(clk)
begin
    if (clk'event and clk='1') then
        if (hsc = 1024+24-1) then
            hs <= '0';
        elsif (hsc = 1024+24+136-1) then
            hs <= '1';
        else
            hs <= hs;
        end if;
        if (hsc = 1344-1) then
            hsc <= (others => '0');
        else
            hsc <= hsc + 1;
        end if;
    end if;
end process;

counter_VS: process(clk)
begin
    if (clk'event and clk='1') then
        if (hsc = 1344-1) then 
            if (vsc = 768+3-1) then
                vs <= '0';
            elsif (vsc = 768+3+6-1) then
                vs <= '1';
            else
                vs <= vs;
            end if;
            if (vsc = 806-1) then
                vsc <= (others => '0');
            else
                vsc <= vsc + 1;
            end if;
        end if;
    end if;
end process;


end Behavioral;
