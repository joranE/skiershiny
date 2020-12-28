with ath as (
  select public.v_sprint_heats.eventid
        ,public.v_sprint_heats.eventid_sf
        ,public.v_sprint_heats.heat
        ,public.v_sprint_heats.compid as ath_compid
        ,public.v_sprint_heats.fisid as ath_fisid
        ,public.v_sprint_heats.name as ath_name
        ,public.v_sprint_heats.nation as ath_nation
        ,public.v_sprint_heats.heat_time as ath_heat_time
        ,public.v_sprint_heats.heat_rank as ath_heat_rank
        ,public.v_sprint_heats.ll as ath_ll
  from public.v_sprint_heats
  where public.v_sprint_heats.compid = {ath_compid}
), opp as (
  select public.v_sprint_heats.eventid
        ,public.v_sprint_heats.compid as opp_compid
        ,public.v_sprint_heats.eventid_sf
        ,public.v_sprint_heats.fisid as opp_fisid
        ,public.v_sprint_heats.name as opp_name
        ,public.v_sprint_heats.nation as opp_nation
        ,public.v_sprint_heats.heat
        ,public.v_sprint_heats.heat_time as opp_heat_time
        ,public.v_sprint_heats.heat_rank as opp_heat_rank
        ,public.v_sprint_heats.ll as opp_ll
  from public.v_sprint_heats
  where public.v_sprint_heats.compid in ({opp_compid*})
)

select ath.*
      ,opp.opp_compid
      ,opp.opp_fisid
      ,opp.opp_name
      ,opp.opp_nation
      ,opp.opp_heat_time
      ,opp.opp_heat_rank
      ,opp.opp_ll
from ath inner join opp on ath.eventid = opp.eventid and ath.heat = opp.heat
