
create view shipbuildview as
select sb.id id, sec.name secname, secretary_lv seclv, ship.name shipname,headquarters_lv hqlv, posted,fuel,ammunition amm,steel,bauxite baux from shipbuild sb
inner join ship on ship.id=sb.ship_id
inner join ship sec on sec.id=sb.secretary
inner join resource rs on sb.resource_id=rs.id;
