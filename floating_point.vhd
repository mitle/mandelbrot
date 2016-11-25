----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/21/2016 02:14:32 PM
-- Design Name: 
-- Module Name: floating_point - Behavioral
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
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity floating_point is
--  Port ( );
end floating_point;

architecture Behavioral of floating_point is

COMPONENT floating_point_0
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_a_tvalid : IN STD_LOGIC;
    s_axis_a_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    s_axis_b_tvalid : IN STD_LOGIC;
    s_axis_b_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    m_axis_result_tvalid : OUT STD_LOGIC;
    m_axis_result_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END COMPONENT;

COMPONENT floating_point_1
  PORT (
    aclk : IN STD_LOGIC;
    s_axis_a_tvalid : IN STD_LOGIC;
    s_axis_a_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    s_axis_b_tvalid : IN STD_LOGIC;
    s_axis_b_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    m_axis_result_tvalid : OUT STD_LOGIC;
    m_axis_result_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
  );
END COMPONENT;

	signal aclk : STD_LOGIC;
    signal a_tvalid :  STD_LOGIC;
    signal a_tdata :  STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal b_tvalid :  STD_LOGIC;
    signal b_tdata :  STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal c_tvalid :  STD_LOGIC;
    signal c_tdata :  STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal result_tvalid :  STD_LOGIC;
    signal result_tdata :  STD_LOGIC_VECTOR(31 DOWNTO 0);
    signal p_result_tvalid :  STD_LOGIC;
    signal p_result_tdata :  STD_LOGIC_VECTOR(31 DOWNTO 0);

begin

process 
begin
aclk <= '0';
wait for 5 ns;

aclk <= '1';
wait for 5 ns;

end process;


process
begin
wait for 100 ns;
wait until aclk'event and aclk = '1' ;
a_tdata <= X"40000000";
a_tvalid <= '1';

b_tdata <= X"40400000";
b_tvalid <= '1';

c_tdata <= X"40800000";
c_tvalid <= '1';

wait until aclk'event and aclk = '1' ;
a_tvalid <= '0';
b_tvalid <= '0';

wait;
end process;

mult_1 : floating_point_1
  PORT MAP (
    aclk => aclk,
    s_axis_a_tvalid => a_tvalid,
    s_axis_a_tdata => a_tdata,
    s_axis_b_tvalid => b_tvalid,
    s_axis_b_tdata => b_tdata,
    m_axis_result_tvalid => p_result_tvalid,
    m_axis_result_tdata => p_result_tdata
  );
  
  add_1 : floating_point_0
    PORT MAP (
      aclk => aclk,
      s_axis_a_tvalid => p_result_tvalid,
      s_axis_a_tdata => p_result_tdata,
      s_axis_b_tvalid => c_tvalid,
      s_axis_b_tdata => c_tdata,
      m_axis_result_tvalid => result_tvalid,
      m_axis_result_tdata => result_tdata
    );

end Behavioral;
