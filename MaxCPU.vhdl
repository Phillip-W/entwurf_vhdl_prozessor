use work.Package_MaxCPU.all;

entity MaxCPU_PU is
  generic (
    Gdata_width: LimitedWidthNatural:= bus_width;
    Gaddress_width: LimitedWidthNatural:= address_width
  );
  port (
    clock : in bit;
    reset: in bit;
    databus_in: in bit_vector (Gdata_width downto 0);
    databus_out: out bit_vector (Gdata_width downto 0);
    addressbus: out bit_vector (Gaddress_width downto 0)
  );
end MaxCPU_PU;


architecture Connectivity_PU of MaxCPU_PU is
  signal switch_memory_devices, write_access_2_memory, access_2_devices, read_write_devices, ready_2_read_write_device : bit;
  begin
end Connectivity_PU;
