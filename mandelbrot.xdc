# ----------------------------------------------------------------------------
# User LEDs - Bank 33
# ---------------------------------------------------------------------------- 
set_property PACKAGE_PIN T22 [get_ports {led[0]}];  # "LD0"
set_property PACKAGE_PIN T21 [get_ports {led[1]}];  # "LD1"
set_property PACKAGE_PIN U22 [get_ports {led[2]}];  # "LD2"
#set_property PACKAGE_PIN U21 [get_ports {LD3}];  # "LD3"
#set_property PACKAGE_PIN V22 [get_ports {LD4}];  # "LD4"
#set_property PACKAGE_PIN W22 [get_ports {LD5}];  # "LD5"
#set_property PACKAGE_PIN U19 [get_ports {LD6}];  # "LD6"
#set_property PACKAGE_PIN U14 [get_ports {LD7}];  # "LD7"

# ----------------------------------------------------------------------------
# VGA Output - Bank 33
# ---------------------------------------------------------------------------- 
set_property PACKAGE_PIN Y21  [get_ports {blue[0]}];  # "VGA-B1"
set_property PACKAGE_PIN Y20  [get_ports {blue[1]}];  # "VGA-B2"
set_property PACKAGE_PIN AB20 [get_ports {blue[2]}];  # "VGA-B3"
set_property PACKAGE_PIN AB19 [get_ports {blue[3]}];  # "VGA-B4"
set_property PACKAGE_PIN AB22 [get_ports {green[0]}];  # "VGA-G1"
set_property PACKAGE_PIN AA22 [get_ports {green[1]}];  # "VGA-G2"
set_property PACKAGE_PIN AB21 [get_ports {green[2]}];  # "VGA-G3"
set_property PACKAGE_PIN AA21 [get_ports {green[3]}];  # "VGA-G4"
set_property PACKAGE_PIN AA19 [get_ports {hs_o}];  # "VGA-HS"
set_property PACKAGE_PIN V20  [get_ports {red[0]}];  # "VGA-R1"
set_property PACKAGE_PIN U20  [get_ports {red[1]}];  # "VGA-R2"
set_property PACKAGE_PIN V19  [get_ports {red[2]}];  # "VGA-R3"
set_property PACKAGE_PIN V18  [get_ports {red[3]}];  # "VGA-R4"
set_property PACKAGE_PIN Y19  [get_ports {vs_o}];  # "VGA-VS"

# ----------------------------------------------------------------------------
# User Push Buttons - Bank 34
# ---------------------------------------------------------------------------- 
set_property PACKAGE_PIN P16 [get_ports {zoom_btn}];  # "BTNC"
set_property PACKAGE_PIN R16 [get_ports {move_btn[0]}];  # "BTND"
set_property PACKAGE_PIN N15 [get_ports {move_btn[2]}];  # "BTNL"
set_property PACKAGE_PIN R18 [get_ports {move_btn[3]}];  # "BTNR"
set_property PACKAGE_PIN T18 [get_ports {move_btn[1]}];  # "BTNU"

# ----------------------------------------------------------------------------
# User DIP Switches - Bank 35
# ---------------------------------------------------------------------------- 
set_property PACKAGE_PIN F22 [get_ports {disable[0]}];  # "SW0"
set_property PACKAGE_PIN G22 [get_ports {disable[1]}];  # "SW1"
set_property PACKAGE_PIN H22 [get_ports {disable[2]}];  # "SW2"
#set_property PACKAGE_PIN F21 [get_ports {SW3}];  # "SW3"
#set_property PACKAGE_PIN H19 [get_ports {SW4}];  # "SW4"
#set_property PACKAGE_PIN H18 [get_ports {SW5}];  # "SW5"
set_property PACKAGE_PIN H17 [get_ports {reset}];  # "SW6"
set_property PACKAGE_PIN M15 [get_ports {start_btn}];  # "SW7"


# ----------------------------------------------------------------------------
# Clock Source - Bank 13
# ---------------------------------------------------------------------------- 
set_property PACKAGE_PIN Y9 [get_ports {clk_board}];  # "GCLK"

# ----------------------------------------------------------------------------
# IOSTANDARD Constraints
#
# Note that these IOSTANDARD constraints are applied to all IOs currently
# assigned within an I/O bank.  If these IOSTANDARD constraints are 
# evaluated prior to other PACKAGE_PIN constraints being applied, then 
# the IOSTANDARD specified will likely not be applied properly to those 
# pins.  Therefore, bank wide IOSTANDARD constraints should be placed 
# within the XDC file in a location that is evaluated AFTER all 
# PACKAGE_PIN constraints within the target bank have been evaluated.
#
# Un-comment one or more of the following IOSTANDARD constraints according to
# the bank pin assignments that are required within a design.
# ---------------------------------------------------------------------------- 

# Note that the bank voltage for IO Bank 33 is fixed to 3.3V on ZedBoard. 
set_property IOSTANDARD LVCMOS33 [get_ports -of_objects [get_iobanks 33]];

# Set the bank voltage for IO Bank 34 to 1.8V by default.
# set_property IOSTANDARD LVCMOS33 [get_ports -of_objects [get_iobanks 34]];
# set_property IOSTANDARD LVCMOS25 [get_ports -of_objects [get_iobanks 34]];
set_property IOSTANDARD LVCMOS18 [get_ports -of_objects [get_iobanks 34]];

# Set the bank voltage for IO Bank 35 to 1.8V by default.
# set_property IOSTANDARD LVCMOS33 [get_ports -of_objects [get_iobanks 35]];
# set_property IOSTANDARD LVCMOS25 [get_ports -of_objects [get_iobanks 35]];

set_property IOSTANDARD LVCMOS18 [get_ports -of_objects [get_iobanks 35]];

# Note that the bank voltage for IO Bank 13 is fixed to 3.3V on ZedBoard. 
set_property IOSTANDARD LVCMOS33 [get_ports -of_objects [get_iobanks 13]];
