library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mandelbrot_tb is
--  Port ( );
end mandelbrot_tb;

architecture Behavioral of mandelbrot_tb is

component mandelbrot is
generic(  sizeX          : integer := 512;  
          sizeY          : integer := 512);
Port (    red       : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
          green     : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
          blue      : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
          hs_o      : out STD_LOGIC;
          vs_o      : out STD_LOGIC;
          clk_board : in STD_LOGIC := '0';
          reset     : in STD_LOGIC := '0';
          led       : out STD_LOGIC_VECTOR (2 downto 0) := "000";
          button    : in STD_LOGIC := '0';
          disable   : in STD_LOGIC_VECTOR (2 downto 0) := "000");
end component mandelbrot;

signal red       : STD_LOGIC_VECTOR (3 downto 0) := "0000";
signal green     : STD_LOGIC_VECTOR (3 downto 0) := "0000";
signal blue      : STD_LOGIC_VECTOR (3 downto 0) := "0000";
signal hs_o      : STD_LOGIC;
signal vs_o      : STD_LOGIC;
signal clk_board : STD_LOGIC := '0';
signal reset     : STD_LOGIC := '1';
signal led       : STD_LOGIC_VECTOR (2 downto 0) := "000";
signal button    : STD_LOGIC := '0';
signal disable   : STD_LOGIC_VECTOR (2 downto 0) := "000";


begin



uut: mandelbrot 
generic map(
    sizeX => 32,
    sizeY => 32)
PORT MAP(
    red        => red,
    green      => green,
    blue       =>  blue,
    hs_o       => hs_o,
    vs_o       => vs_o,
    clk_board  => clk_board,
    reset      => reset,
    led        => led,
    button     => button,
    disable    => disable
);


--5ns for 100MHz clock
clock: process 
begin
    clk_board <= '0';
	wait for 5 ns;
	clk_board <= '1';
	wait for 5 ns;
end process;


stim: process
begin
	wait for 100 ns;
	
	reset <= '0';
	wait for 5000 ns;
		
	button <= '1';
	wait for 100 ns;
	
	button <= '0'; 
	wait;
end process;


end Behavioral;
