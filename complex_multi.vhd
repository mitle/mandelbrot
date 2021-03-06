----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/18/2016 04:29:01 PM
-- Design Name: 
-- Module Name: complex_multi - Behavioral
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
use IEEE.STD_LOGIC_ARITH.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity complex_multi is
	Port( clk : in STD_LOGIC;
       	  reset : in STD_LOGIC;
          hso: out std_logic;
          vso: out std_logic;
          red: out std_logic_vector(3 downto 0);
          blue:out std_logic_vector(3 downto 0);
          green:out std_logic_vector(3 downto 0)
          );
end complex_multi;

package complex_numbers is
	constant Length_complex : Double := 32;
	--32 bites complex
	type Complex32 is record
		R : Signed(Lenght_complex - 1 downto 0);
		I : Signed(Lenght_complex - 1 downto 0);
	end record;
	--64 bites complex
	type Complex64 is record
			R : Signed(2*Lenght_complex - 1 downto 0);
			I : Signed(2*Lenght_complex - 1 downto 0);
	end record;
end complex_numbers;
--body
package body complex_numbers is
	--32 bites összeadás túlterhelés
	function "+" (A: Complex32; 
				  B: Complex32) 
	return Complex32 is variable V: Complex32;
	begin
		V.R := A.R + B.R;
		V.I := A.I + B.I;
		return V;
	end "+";
	--64 bites összeadás túlterhelés
	function "+" (A: Complex64; 
				  B: Complex64) 
		return Complex64 is variable V: Complex64;
		begin
			V.R := A.R + B.R;
			V.I := A.I + B.I;
			return V;
	end "+";
	--szorzás túlterhelés
	function "*" (A: Complex32; 
				  B: Complex32)
		return Complex64 is variable V: Complex64;
		begin 
			V.R := A.R*B.R - (A.I*B.I);
			V.I := A.I*B.R + (A.R*B.I);
			return V;
	end "*";
end complex_numbers;

	signal hc :unsigned (10 downto 0); 
	signal vc :unsigned (9 downto 0); 
	signal hs :STD_LOGIC;
	signal vs :STD_LOGIC;
	signal clk_65 :STD_LOGIC;


architecture Behavioral of complex_multi is

begin	
process(clk_65) begin
	if ( clk_65'event and clk_65 = '1') then
		--hc,vc beállítás
		if (hc = 1344-1) then 
			hc <= (others => '0');
			if (vc = 806-1) then
				vc <= (others => '0');
			else
				vc <= vc+1;
			end if;
		else
			hc <= hc+1;
		end if;
		--hs beállítás
		if ( hc = 1024+24-1 ) then
			hs <= '0';
		elsif ( hc = 1024+24+136-1 ) then
			hs <= '1';
		else
			hs <= hs;
		end if;
		--vs beállítás	
		if ( vc = 768+3-1 ) then
			vs <= '0';
		elsif ( vc = 768+3+6-1 ) then
			vs <= '1';
		else
			vs <= vs;
		end if;	
	end if;
end process;

signal nx : double:= 4.0;
signal z : Complex32;
signal c : Complex32;
signal t : integer;

process(mandel_calc) begin
	for x in 0 to 512 loop
		for y in 0 to 512 loop
			c.r <= (2/512)*x;
			c.i <= (2/512)*y + 1;
			z.r <= 0.0;
			z.i <= 0.0;
			t <= 0;
			while (z.r*z.r + z.i*z.i) < 4.0 and t < 255 loop
				z <= z*z + c;
				t <= t + 1;
			end loop;
			t = t*15;
			--memória kell ide
		end loop;
	end loop;
end process;



end Behavioral;
