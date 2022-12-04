create view "round_count" ("round_count") as select length(select "input" from "input") / 5;

create view "round" ("num", "code1", "code2") as
	select "value", substr("input", "value" * 5 + 1, 1), substr("input", "value" * 5 + 3, 1)
	from "input" join generate_series(0, (select "round_count" from "round_count") - 1);

create view "move" ("name", "beats", "code1", "code2", "score") as (values
	('rock', 'scissors', 'A', 'X', 1),
	('paper', 'rock', 'B', 'Y', 2),
	('scissors', 'paper', 'C', 'Z', 3)
);

create view "outcome" ("name", "score", "code2") as (values
	('lose', 0, 'X'),
	('draw', 3, 'Y'),
	('win', 6, 'Z')
);

create view "moves_outcome" ("self_move", "other_move", "outcome") as
	select "self_move"."name", "other_move"."name", case
		when "self_move"."name" = "other_move"."name" then 'draw'
		when "self_move"."beats" = "other_move"."name" then 'win'
		when "other_move"."beats" = "self_move"."name" then 'lose'
	end from "move" as "self_move" join "move" as "other_move";

create view "round_p1" ("num", "self_move", "other_move", "outcome", "score") as
	select
		"round"."num", "self_move"."name", "other_move"."name", "outcome"."name",
		"self_move"."score" + "outcome"."score"
	from "round"
	join "move" as "other_move" on "round"."code1" = "other_move"."code1"
	join "move" as "self_move" on "round"."code2" = "self_move"."code2"
	join "moves_outcome" on
		"self_move"."name" = "moves_outcome"."self_move"
		and "other_move"."name" = "moves_outcome"."other_move"
	join "outcome" on "outcome"."name" = "moves_outcome"."outcome";

create view "csv_p1" ("answer") as
	select group_concat(
		"self_move" || ',' || "other_move" || ',' || "outcome" || ',' || "score", char(10)
	)
	from (select * from "round_p1" order by "num");

create view "p2" ("answer") as select sum("score") from "round_p1";

create view "round_p2" ("num", "self_move", "other_move", "outcome", "score") as
	select
		"round"."num", "self_move"."name", "other_move"."name", "outcome"."name",
		"self_move"."score" + "outcome"."score"
	from "round"
	join "move" as "other_move" on "round"."code1" = "other_move"."code1"
	join "outcome" on "round"."code2" = "outcome"."code2"
	join "moves_outcome" on
		"other_move"."name" = "moves_outcome"."other_move" 
		and "outcome"."name" = "moves_outcome"."outcome"
	join "outcome" on "outcome"."name" = "moves_outcome"."outcome";

create view "csv_p2" ("answer") as
	select group_concat(
		"self_move" || ',' || "other_move" || ',' || "outcome" || ',' || "score", char(10)
	)
	from (select * from "round_p2" order by "num");

create view "p2" ("answer") as select sum("score") from "round_p2";