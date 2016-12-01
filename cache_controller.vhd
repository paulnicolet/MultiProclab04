library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity CacheController is

  port (
    clk, rst                       : in    std_logic;
    cacheCs, cacheRead, cacheWrite : in    std_logic;
    cacheAddr                      : in    std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    cacheWrData                    : in    data_word_t;
    cacheDone                      : out   std_logic;
    cacheRdData                    : out   data_word_t;
    busReq                         : out   std_logic;
    busCmd                         : inout bus_cmd_t;
    busSnoopValid                  : in    std_logic;
    busGrant                       : in    std_logic;
    busAddr                        : inout std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    busData                        : inout data_block_t);
end entity CacheController;

architecture rtl of CacheController is

  type cache_ctrl_state_t is (ST_IDLE, ST_RD_HIT_TEST, ST_RD_WAIT_BUS_GRANT,
                              ST_RD_WAIT_BUS_COMPLETE,
                              ST_WR_HIT_TEST, ST_WR_WAIT_BUS_GRANT, ST_WR_WAIT_BUS_COMPLETE);
  signal cacheStNext, cacheSt : cache_ctrl_state_t := ST_IDLE;

  type snoop_state_t is (ST_SNOOP_IDLE, ST_SNOOP_INVALIDATING);
  signal snoopStNext, snoopSt : snoop_state_t := ST_SNOOP_IDLE;

  -- tag Array
  signal tagLookupEn, tagWrEn, tagInvEn                                    : std_logic;
  signal tagWrSet                                                          : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal tagAddr, tagInvAddr                                               : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal tagHitEn                                                          : std_logic;
  signal tagHitSet, tagVictimSet                                           : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  -- data array
  signal dataArrayWrEn, dataArrayWrWord                                    : std_logic;
  signal dataArrayWrSetIdx                                                 : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal dataArrayAddr                                                     : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal dataArrayWrData                                                   : data_block_t;
  signal dataArrayRdData                                                   : data_set_t;

  -- bus tri state buffer
  signal busOutEn  : std_logic;
  signal busAddrIn : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal busCmdIn  : bus_cmd_t;
  signal busDataIn : data_block_t;
-- 
begin  -- architecture rtl

  comb_proc : process () is
  begin  -- process comb_proc
    -- signals that need initialization
    cacheStNext <= cacheSt;
    cacheDone   <= '0';

    cpuReqRegWrEn <= '0';
    victimRegWrEn <= '0';

    tagLookupEn <= '0';
    tagWrEn     <= '0';

    dataArrayWrEn   <= '0';
    dataArrayWrWord <= '0';

    busReq   <= '0';
    busOutEn <= '0';


    -- signals with dont care initialization
    cacheRdData   <= (others => 'Z');
    dataArrayAddr <= cacheAddr;

    busCmdIn     <= BUS_READ;
    busAddrIn    <= cpuReqReg.addr;
    busDataIn(0) <= cpuReqReg.data;
    busDataIn(1) <= cpuReqReg.data;

    tagWrSet <= victimReg.set;
    tagAddr  <= cpuReqReg.addr;

    dataArrayWrData   <= busData;
    dataArrayAddr     <= cpuReqReg.addr;
    dataArrayWrSetIdx <= tagVictimSet;

    -- control: state machine
    case cacheSt is
      when ST_IDLE =>

      -----------------------------------------------------------------------
      -- Rdb state machine
      -----------------------------------------------------------------------
      when ST_RD_HIT_TEST =>

      when ST_RD_WAIT_BUS_GRANT =>

      when ST_RD_WAIT_BUS_COMPLETE =>

      -----------------------------------------------------------------------
      -- wr state machine
      -----------------------------------------------------------------------
      when ST_WR_HIT_TEST =>
        
      when ST_WR_WAIT_BUS_GRANT =>

      when ST_WR_WAIT_BUS_COMPLETE =>

      when others => null;
    end case;

    -----------------------------------------------------------------------
    -- snoop state machine
    -----------------------------------------------------------------------
    snoopStNext <= snoopSt;
    tagInvEn    <= '0';
    case snoopSt is
      when ST_SNOOP_IDLE =>
      when ST_SNOOP_INVALIDATING =>
      when others => null;
    end case;

    -- datapath:
  end process comb_proc;

  TagArray_1 : TagArray
    port map (
      clk          => clk,
      rst          => rst,
      tagLookupEn  => tagLookupEn,
      tagWrEn      => tagWrEn,
      tagInvEn     => tagInvEn,
      tagWrSet     => tagWrSet,
      tagAddr      => tagAddr,
      tagInvAddr   => busAddr,
      tagHitEn     => tagHitEn,
      tagHitSet    => tagHitSet,
      tagVictimSet => tagVictimSet);

  DataArray_1 : DataArray
    port map (
      clk               => clk,
      dataArrayWrEn     => dataArrayWrEn,
      dataArrayWrWord   => dataArrayWrWord,
      dataArrayWrSetIdx => dataArrayWrSetIdx,
      dataArrayAddr     => dataArrayAddr,
      dataArrayWrData   => dataArrayWrData,
      dataArrayRdData   => dataArrayRdData);

  clk_proc : process (clk, rst) is
  begin  -- process clk_proc
    if rst = '0' then                   -- asynchronous reset (active low)
      cacheSt <= ST_IDLE;
    elsif clk'event and clk = '1' then  -- rising clock edge
      cacheSt <= cacheStNext;
      snoopSt <= snoopStNext;

      -- other regs go here
    end if;
  end process clk_proc;

end architecture rtl;
