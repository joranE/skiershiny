with ath as (
  select dst_result.*,
        case when t1.prim_tag in ('wc','tds','wsc','owg') then event_penalty.penalty + (dst_result.pbm / event_penalty.pbm_sd)
			       else event_penalty.penalty + (dst_result.pbm / (1.5 * event_penalty.pbm_sd))
		    end as pbm_pts
  from dst_result left outer join dst_event on dst_result.eventid = dst_event.eventid
                  left outer join event_penalty on dst_result.eventid = event_penalty.eventid
                  left outer join (select eventid,tag as prim_tag from event_tag where primary_tag = 'Y') t1 on dst_result.eventid = t1.eventid
  where compid = {ath_compid} and dst_result.rank is not null
), opp as (
  select dst_result.eventid
        ,compid as opp_compid
        ,nation as opp_nation
        ,rank as opp_rank
        ,time as opp_time
        ,pb as opp_pb
        ,pbm as opp_pbm
        ,case when t1.prim_tag in ('wc','tds','wsc','owg') then event_penalty.penalty + (dst_result.pbm / event_penalty.pbm_sd)
			       else event_penalty.penalty + (dst_result.pbm / (1.5 * event_penalty.pbm_sd))
		    end as opp_pbm_pts
        ,fispoints as opp_fispoints
  from dst_result left outer join dst_event on dst_result.eventid = dst_event.eventid
                  left outer join event_penalty on dst_result.eventid = event_penalty.eventid
                  left outer join (select eventid,tag as prim_tag from event_tag where primary_tag = 'Y') t1 on dst_result.eventid = t1.eventid
  where compid in ({opp_compid*}) and dst_result.rank is not null
)

select ev.season
      ,ev.date
      ,ev.location
      ,ev.site
      ,ev.length
      ,ev.tech
      ,ev.format
      ,t1.prim_tag as primary_tag
      ,case when exists (select 1 from dst_splits where dst_splits.eventid = ev.eventid) then 'Y' else 'N' end as splits_exist
      ,ath.eventid
      ,ath.compid as ath_compid
      ,s1.name as ath_name
      ,ath.nation as ath_nation
      ,ath.rank as ath_rank
      ,ath.time as ath_time
      ,ath.pb as ath_pb
      ,ath.pbm as ath_pbm
      ,ath.pbm_pts as ath_pbm_pts
      ,ath.fispoints as ath_fispoints
      ,opp.opp_compid
      ,s2.name as opp_name
      ,opp.opp_nation
      ,opp.opp_rank
      ,opp.opp_time
      ,opp.opp_pb
      ,opp.opp_pbm
      ,opp.opp_pbm_pts
      ,opp.opp_fispoints
from ath inner join opp on ath.eventid = opp.eventid
         left outer join dst_event ev on ath.eventid = ev.eventid
         left outer join skier s1 on ath.compid = s1.compid
         left outer join skier s2 on opp.opp_compid = s2.compid
         left outer join (select eventid,tag as prim_tag from event_tag where primary_tag = 'Y') t1 on ev.eventid = t1.eventid
