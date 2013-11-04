
create view equipmentview as
select 1 id, eq.id equipid, eq.name name, rs.fuel fuel, rs.ammunition amm, rs.steel steel, rs.bauxite baux, count(dev.resource_id) count, rcv.count total from equipment eq
inner join develop dev on eq.id = dev.equipment_id
inner join recipecountsview rcv on rcv.id = dev.resource_id
inner join resource rs on rs.id = dev.resource_id
group by dev.equipment_id, dev.resource_id;
