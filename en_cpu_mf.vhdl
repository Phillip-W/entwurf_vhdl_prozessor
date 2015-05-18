LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
ENTITY CPU IS
  GENERIC(
    data_width : positive; -- Parameter in package auslagern?  1.1.1
    address_width : positive); -- Parameter wieder auslagern.  1.1.2
  PORT(
    clk, res, c_m_io, c_wa, c_io, i_sr : IN bit;            --1.2.1, 1.2.2, 1.2.6, 1.2.7, 1.2.8, 1.2.10
    dbus_m_io, dbus_mem, abus_mem : IN bit_vector;            --1.2.3, 1.2.4, 1.2.5
    d_io : OUT bit);
END CPU;
