----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/25/2016 04:31:58 PM
-- Design Name: 
-- Module Name: state - Behavioral
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


entity state is
Port (    red       : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
          green     : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
          blue      : out STD_LOGIC_VECTOR (3 downto 0) := "0000";
          hs_o      : out STD_LOGIC;
          vs_o      : out STD_LOGIC;
          clk_board : in STD_LOGIC := '0';
          reset     : in STD_LOGIC := '0';
          disable   : in STD_LOGIC_VECTOR (2 downto 0) := "000");
end state;


architecture Behavioral of state is

component vga_clk_gen
port (
  clk_in1           : in     std_logic;
  clk_out1          : out    std_logic
 );
end component;



type allapot is (iter,pixel,feltetel_1,feltetel_2,szin,ram);

signal nx : double:= 4.0;
signal zr : double;
signal zi : double;
signal cr : double;
signal ci : double;
signal t : double;
signal state, next_state :allapot;

--RAM
constant C_RAM_WIDTH : integer := 12;            		-- Specify RAM data width
constant C_RAM_DEPTH : integer := 512*512; -- Specify RAM depth (number of entries) 
signal clk_65  : std_logic;                       			  -- Clock
signal wea   : std_logic;                       			  -- Write enable
signal doutb : std_logic_vector(C_RAM_WIDTH-1 downto 0); 		  -- RAM output data
type ram_type is array (C_RAM_DEPTH-1 downto 0) of std_logic_vector (C_RAM_WIDTH-1 downto 0);          -- 2D Array Declaration for RAM signal
signal ram_data : std_logic_vector(C_RAM_WIDTH-1 downto 0) ;
signal ram_name : ram_type := init_from_file_or_zeroes(C_INIT_FILE);

--XGA Signal 1024 x 768 @ 60 Hz timing - (65.0 MHz Pixel freq.)
--horizontal timings (pixels)
constant hor_visual      : integer := 1024;
constant hor_front_porch : integer := 24;
constant hor_sync        : integer := 136;
constant hor_back_porch  : integer := 160;

--vertical timings (lines)
constant ver_visual      : integer := 768;
constant ver_front_porch : integer := 3;
constant ver_sync        : integer := 6;
constant ver_back_porch  : integer := 29;

 
signal clk, hs, vs  : STD_LOGIC := '0';
signal hsc          : unsigned(11 downto 0) := (others => '0');
signal vsc          : unsigned(10 downto 0) := (others => '0');
signal tmp          : STD_LOGIC_VECTOR(11 downto 0) := (others => '0');
signal tmp_H        : STD_LOGIC_VECTOR(5 downto 0) := (others => '0');
signal tmp_L        : STD_LOGIC_VECTOR(5 downto 0) := (others => '0');
signal enable_out : STD_LOGIC := '0';



begin




OUTPUT_DECODE: process (state)
begin
	if state = start then
	end if;
	
	if state = pixel then 
	 --c kiszámolás a pozició alapján
	 --z nullázás
	 --t nullázás
	 end if;
	 
	 if state = feltetel_1 then
	 --z.r * z.r  + z.i*z.i  < 4
	 end if;
	 
	 if state = feltetel_2 then
	 --t< 255
	 end if;
	 
	 if state = szin then
	 --új  z számolás
	 end if;
	 
	 if state = ram then
	 --memóriába írás
	 end if;
end process;

NEXT_STATE_DECODE: process ()
begin
next_state <= state;
 case (state) is
	--inicializáljuk az első pixel helyét
	when start =>
	   if  "start" then
		  next_state <= pixel;
	   end if;
					  
	when pixel =>
	   if "feltetel1" then
		  next_state <= feltetel_1;
	   end if;
		 
	when feltetel_1 =>
		if "igaz" then
			next_state <= feltetel_2;
		elsif "hamis" then
			next_state <= szin;
		end if;
		
   when feltetel_2=>
		if "igaz" then
			next_state <= pixel;
		elsif "hamis" then
			next_state <= szin;
		end if;
		
	when szin =>
		if "számolás" then
			next_state <= ram;
		end if;
	   
	when others =>
		if "valami" then
		  next_state <= start;
		end if;
 end case;      
end process;
 
--RAM      
process(clk)
begin
  if(clk'event and clk = '1') then
	  if(wea = '1') then
		  ram_name(to_integer(unsigned(addra))) <= dina;
	  end if;
	  --if(enb = '1') then
		  ram_data <= ram_name(to_integer(unsigned(hc(8 downto 0) & vc(8 downto 0))));
	  --end if;
  end if;
end process;

enable_out_proc: process(clk, reset)
begin
   if (reset = '1') then
       enable_out <= '0';
   elsif (clk'event and clk='1') then
       if(hsc = 0) then
           enable_out <= '1';
       elsif(hsc = hor_visual or vsc = ver_visual) then
           enable_out <= '0';
       else
           enable_out <= enable_out;
       end if;
   end if;
end process;

--tmp_H <= std_logic_vector(hsc(6 downto 3)) & std_logic_vector(vsc(5 downto 4));
--tmp_L <= std_logic_vector(vsc(6 downto 1)) XOR  ( std_logic_vector(vsc(5 downto 0)) AND  std_logic_vector(hsc(6 downto 1)) );
--tmp   <= tmp_H & tmp_L;

pattern: process(clk)
begin
    if (clk'event and clk='1') then
        if( enable_out = '1' ) then
        --if( hsc < hor_visual and vsc < ver_visual) then
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
        if (hsc = hor_visual+hor_front_porch-1) then
            hs <= '0';
        elsif (hsc = hor_visual+hor_front_porch+hor_sync-1) then
            hs <= '1';
        else
            hs <= hs;
        end if;
        if (hsc = hor_visual+hor_front_porch+hor_sync+hor_back_porch-1) then
            hsc <= (others => '0');
        else
            hsc <= hsc + 1;
        end if;
    end if;
end process;

counter_VS: process(clk)
begin
    if (clk'event and clk='1') then
        if (hsc = hor_visual+hor_front_porch+hor_sync+hor_back_porch-1) then 
            if (vsc = ver_visual+ver_front_porch-1) then
                vs <= '0';
            elsif (vsc = ver_visual+ver_front_porch+ver_sync-1) then
                vs <= '1';
            else
                vs <= vs;
            end if;
            if (vsc = ver_visual+ver_front_porch+ver_sync+ver_back_porch-1) then
                vsc <= (others => '0');
            else
                vsc <= vsc + 1;
            end if;
        end if;
    end if;
end process;

end Behavioral;
