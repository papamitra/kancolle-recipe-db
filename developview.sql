
create view developview as
select dev.id id, sec.name secname, secretary_lv seclv, eq.name equipname,headquarters_lv hqlv, posted,fuel,ammunition amm,steel,bauxite baux from develop dev
left join equipment eq on eq.id=dev.equipment_id
inner join ship sec on sec.id=dev.secretary
inner join resource rs on dev.resource_id=rs.id;
