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
  signal tagLookupEn, tagWrEn, tagInvEn     : std_logic;
  signal tagWrSet                           : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal tagAddr, tagInvAddr                : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal tagHitEn                           : std_logic;
  signal tagHitSet, tagVictimSet            : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  -- data array
  signal dataArrayWrEn, dataArrayWrWord     : std_logic;
  signal dataArrayWrSetIdx                  : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal dataArrayAddr                      : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal dataArrayWrData                    : data_block_t;
  signal dataArrayRdData                    : data_set_t;

  ------------------------------ Signals we added ------------------------------
  -- datapath independant signals
  signal busWillInvalidate        : std_logic;
  signal currReqWillInvalidate    : std_logic;
  signal cpuReqRegWillInvalidate  : std_logic:

  -- cpuRegReq input/output
  signal cpuReqRegWrEn            : std_logic;
  signal cpuReqRegWasInvalidatedIn : std_logic;
  signal cpuReqRegAddrIn          : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal cpuReqRegDataIn          : data_block_t;

  signal cpuReqRegWasInvalidated   : std_logic;
  signal cpuReqRegWord            : std_logic;
  signal cpuReqRegData            : data_block_t;

  -- VictimReg input/output
  signal victimRegWrEn            : std_logic;
  signal victimRegSetIn           : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal victimRegSet             : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);

  -- RdDataTriStateBuffer input (no outputs because mapped to the cache outputs)
  signal cacheRdOutEn             : std_logic;
  signal cacheRdDataIn            : data_word_t;

  -- BusTriStateBuffer input (no outputs because mapped to the cache outputs)
  signal busOutEn                 : std_logic;
  signal busCmdIn                 : bus_cmd_t;
  signal busDataIn                : data_block_t;
  signal busAddrIn                : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);


  --------------------------- Components declaration ---------------------------

  component BusTriStateBufferForCacheController is
  port (
    busOutEn  : in  std_logic;
    busCmdIn  : in  bus_cmd_t;
    busDataIn : in  data_block_t;
    busAddrIn : in  std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    busCmd    : out bus_cmd_t;
    busData   : out data_block_t;
    busAddr   : out std_logic_vector(WORD_ADDR_WIDTH-1 downto 0)
  );
  end component BusTriStateBufferForCacheController;

  component CpuReqReg is
  port (
    clk                       : in std_logic;
    rst                       : in std_logic;
    cpuReqRegWrEn             : in std_logic;
    cpuReqRegWasInvalidatedIn  : in std_logic;
    cpuReqRegAddrIn           : in std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    cpuReqRegDataIn           : in data_word_t;
    cpuReqRegAddr             : out std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    cpuReqRegWasInvalidated    : out std_logic;
    cpuReqRegData             : out data_block_t);
  end component CpuReqReg;

  component VictimReg is
  port (
    clk                       : in std_logic;
    rst                       : in std_logic;
    victimRegWrEn             : in std_logic;
    victimRegSetIn            : in std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
    victimRegSet              : out std_logic_vector(SET_ADDR_WIDTH-1 downto 0));
  end component VictimReg;

  component RdDataTriStateBuffer is
  port (
    cacheRdOutEn              : in std_logic;
    cacheRdDataIn             : in data_word_t;
    cacheRdData               : out data_word_t);
  end component RdDataTriStateBuffer;


begin  -- architecture rtl
  
  ---------------------------- Combinational process --------------------------- 

  comb_proc : process (snoopSt, busWillInvalidate) is
  begin  -- process comb_proc
    -- signals that need initialization
    cacheStNext <= cacheSt;
    
    cpuReqRegWrEn   <= '0';
    victimRegWrEn   <= '0';
    tagLookupEn     <= '0';
    tagWrEn         <= '0';
    dataArrayWrEn   <= '0';
    dataArrayWrWord <= '0';
    busOutEn        <= '0';
    cacheRdOutEn    <= '0';

    cacheDone       <= '0';
    busReq          <= '0';


    -- signals with dont care initialization
    --cacheRdData   <= (others => 'Z');
    --dataArrayAddr <= cacheAddr;

    --busCmdIn     <= BUS_READ;
    --busAddrIn    <= cpuReqReg.addr;
    --busDataIn(0) <= cpuReqReg.data;
    --busDataIn(1) <= cpuReqReg.data;

    --tagWrSet <= victimReg.set;
    --tagAddr  <= cpuReqReg.addr;

    --dataArrayWrData   <= busData;
    --dataArrayAddr     <= cpuReqReg.addr;
    --dataArrayWrSetIdx <= tagVictimSet;

    -- control: state machine
    case cacheSt is
      when ST_IDLE =>
        if (cacheCs = '0' or cacheWrite = '0') then
          cacheStNext <= ST_IDLE -- not necessary but to make sure we don't forget stuff
        elsif (cacheCs = '1' and cacheWrite = '1') then
          cpuReqRegWrEn <= '1';
          dataArrayAddr <= cacheAddr;
          tagAddr <= cacheAddr;
          tagLookupEn <= '1';
        end if;

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
        if busWillInvalidate = '1' then
          snoopStNext <= ST_SNOOP_INVALIDATING;
          tagInvEn <= '1';
        end if;

      when ST_SNOOP_INVALIDATING =>
        snoopStNext <= ST_SNOOP_IDLE;

      when others => null;
    end case;

    --Datapath extensions (3 boxes at the bottom of the PDF)
    busWillInvalidate       <= (busCmd = BUS_WRITE) and busSnoopValid and (not busGrant);
    currReqWillInvalidate   <= (cacheAddr = busAddr) and busWillInvalidate;
    cpuReqRegWillInvalidate <= ((cpuReqRegAddr = busAddr) and busWillInvalidate) or cpuReqRegWasInvalidated;
  end process comb_proc;

  ---------------------------- Component mapping -------------------------------
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

  BusTriStateBufferForCacheController_1 : BusTriStateBufferForCacheController
  port map (
    busOutEn    => busOutEn,
    busCmdIn    => busCmdIn,
    busDataIn   => busDataIn,
    busAddrIn   => busAddrIn,
    busCmd      => busCmd,
    busData     => busData,
    busAddr     => busAddr);

  VictimReg_1 : VictimReg
  port map (
   clk              => clk,
   rst              => rst,
   victimRegWrEn    => victimRegWrEn,
   victimRegSetIn   => tagVictimSet,
   victimRegSet     => victimRegSet);

  CpuReqReg_1 : CpuReqReg
  port map (
   clk                        => clk,
   rst                        => rst,
   cpuReqRegWrEn              => cpuReqRegWrEn,
   cpuReqRegAddrIn            => cacheAddr,
   cpuReqRegDataIn            => cacheWrData,
   cpuReqRegWasInvalidatedIn  => currReqWillInvalidated, -- TODO : currReqWillInvalidate in the pdf, wouldn't it be more logical with cpuReqRegWillInvalidate ?
   cpuReqRegAddr              => cpuReqRegAddr,
   cpuReqRegWasInvalidated    => cpuReqRegWasInvalidated, 
   cpuReqRegData              => cpuReqRegData
  );

  RdDataTriStateBuffer_1 : RdDataTriStateBuffer
  port map (
   cacheRdOutEn   => cacheRdOutEn,
   cacheRdDataIn  => cacheRdDataIn,
   cacheRdData    => cacheRdData);

  ------------------------------- Clock update ---------------------------------

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


-------------------------- Components implementations --------------------------

----------------------------------- CpuReqReg ----------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity CpuReqReg is

  port (
    clk                       : in std_logic;
    rst                       : in std_logic;
    cpuReqRegWrEn             : in std_logic;
    cpuReqRegWasInvalidatedIn : in std_logic;
    cpuReqRegAddrIn           : in std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    cpuReqRegDataIn           : in data_word_t;
    cpuReqRegAddr             : out std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    cpuReqRegWasInvalidated   : out std_logic;
    cpuReqRegData             : out data_block_t
  );

end entity CpuReqReg;

architecture crr of CpuReqReg is

begin

  process(clk, rst)
  begin
    if (rst = '0') then
      cpuReqRegAddr             <= (others => '0');
      cpuReqRegData             <= DATA_BLOCK_HIGH_IMPEDANCE;
      cpuReqRegWasInvalidated   <=  '0';
    elsif clk'event and clk = '1' then
      if (cpuReqRegWrEn = '1') then
        cpuReqRegAddr           <= cpuReqRegAddrIn;
        cpuReqRegData(0)        <= cpuReqRegDataIn;
        cpuReqRegData(1)        <= (others => '0');
        cpuReqRegWasInvalidated <= cpuReqRegWasInvalidatedIn;
      end if;
    end if;
  end process;

end architecture;

----------------------------------- VictimReg ----------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity VictimReg is

  port (
    clk              : in std_logic;
    rst              : in std_logic;
    victimRegWrEn    : in std_logic;
    victimRegSetIn   : in std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
    victimRegSet     : out std_logic_vector(SET_ADDR_WIDTH-1 downto 0));

end entity VictimReg;

architecture vr of VictimReg is

begin

  process(clk, rst)
  begin
    if (rst = '0') then
      victimRegSet      <= (others => '0');
    elsif clk'event and clk = '1' then
      if (victimRegWrEn = '1') then
        victimRegSet    <= victimRegSetIn;
      end if;
    end if;
  end process;

end architecture;

--------------------- BusTriStateBufferForCacheController ----------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity BusTriStateBufferForCacheController is

  port (
    busOutEn  : in  std_logic;
    busCmdIn  : in  bus_cmd_t;
    busDataIn : in  data_block_t;
    busAddrIn : in  std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    busCmd    : out bus_cmd_t;
    busData   : out data_block_t;
    busAddr   : out std_logic_vector(WORD_ADDR_WIDTH-1 downto 0)
  );

end entity BusTriStateBufferForCacheController;

architecture tsb of BusTriStateBufferForCacheController is

begin
  busData   <= busDataIn when (busOutEn = '1') else DATA_BLOCK_HIGH_IMPEDANCE;
  busCmd    <= busCmdIn  when (busOutEn = '1') else (others => 'Z');
  busAddr   <= busAddrIn when (busOutEn = '1') else (others => 'Z');
end architecture tsb;

----------------------------- RdDataTriStateBuffer -----------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity RdDataTriStateBuffer is

  port (
    cacheRdOutEn  : in std_logic;
    cacheRdDataIn : in data_word_t;
    cacheRdData   : out data_word_t
  );

end entity RdDataTriStateBuffer;

architecture rdtsb of RdDataTriStateBuffer is

begin
  cacheRdData <= cacheRdDataIn when (cacheRdOutEn = '1') else DATA_WORD_HIGH_IMPEDANCE;
end architecture rdtsb;
