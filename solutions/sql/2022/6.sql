create view "p1" ("answer") as
with
	"char" ("index", "char") as (
		select 1, substr("content", 1, 1) from "input"
		union all
		select "index" + 1, substr("content", "index" + 1, 1)
		from "input" join "char" where "index" + 1 <= length("content")
	),
	"range" ("value") as (
		select 1
		union all
		select "value" + 1 from "range" where "value" < 4
	),
	"window" ("base_index", "item_index") as (
		select "index" as "base_index", "index" - "value" as "item_index"
		from "char" join "range" where "item_index" > 0
	),
	"winscore" ("index", "winscore") as (
		select "window"."base_index", count(distinct "char")
		from "window" join "char" on "window"."item_index" = "char"."index" 
		group by "window"."base_index"
	),
	"good_index" ("index") as (select "index" from "winscore" where "winscore" = 4)
select min("index") - 1 from "good_index";

create view "p2" ("answer") as
with
	"char" ("index", "char") as (
		select 1, substr("content", 1, 1) from "input"
		union all
		select "index" + 1, substr("content", "index" + 1, 1)
		from "input" join "char" where "index" + 1 <= length("content")
	),
	"range" ("value") as (
		select 1
		union all
		select "value" + 1 from "range" where "value" < 14
	),
	"window" ("base_index", "item_index") as (
		select "index" as "base_index", "index" - "value" as "item_index"
		from "char" join "range" where "item_index" > 0
	),
	"winscore" ("index", "winscore") as (
		select "window"."base_index", count(distinct "char")
		from "window" join "char" on "window"."item_index" = "char"."index" 
		group by "window"."base_index"
	),
	"good_index" ("index") as (select "index" from "winscore" where "winscore" = 14)
select min("index") - 1 from "good_index";
