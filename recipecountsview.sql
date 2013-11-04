
create view recipecountsview as
select  dev.resource_id id, count(dev.resource_id) count from develop dev
left join equipment eq on eq.id = dev.equipment_id
left join resource rs on rs.id = dev.resource_id
group by dev.resource_id;
