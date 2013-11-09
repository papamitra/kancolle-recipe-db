
create view develrecipeallview as
select rs.id id, count(resource_id) cnt, rs.fuel fuel, rs.ammunition amm, rs.steel steel, rs.bauxite baux from develop dev
left join resource rs on rs.id = dev.resource_id
group by resource_id
order by cnt desc;

