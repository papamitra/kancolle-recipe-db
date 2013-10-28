
create view developrecipeview as
select eq.id id, count(eq.id) equipcount, eq.name equipname, rs.id resourceid from develop dev
left join equipment eq on eq.id=dev.equipment_id
inner join ship sec on sec.id=dev.secretary
inner join resource rs on dev.resource_id=rs.id
group by eq.id;
