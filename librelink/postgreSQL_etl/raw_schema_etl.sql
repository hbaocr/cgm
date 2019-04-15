-- SCHEMA: raw

-- DROP SCHEMA "raw" ;

CREATE SCHEMA "raw"
    AUTHORIZATION postgres;

COMMENT ON SCHEMA "raw"
    IS 'raw tables put here';
	
show search_path;
	
SET search_path TO raw,public;

/* the table stores user account information */
drop table if exists raw.user_account cascade; -- cascade enabled to force drop dependency.
create table raw.user_account(
	user_id serial PRIMARY KEY,
	username VARCHAR (50) UNIQUE NOT NULL,
	password VARCHAR (50) NOT NULL,
	email VARCHAR (355) UNIQUE NOT NULL,
	created_on TIMESTAMP NOT NULL,
	last_login TIMESTAMP
);

/* the table stores cgm_librelink (continuous glucose monitoring) information */
drop table if exists raw.cgm_librelink;
create table raw.cgm_librelink(
	user_id SMALLINT REFERENCES raw.user_account(user_id) ON DELETE RESTRICT,
	meter VARCHAR (50), -- meter brand/manufacturer
	serial_number VARCHAR (355), -- hardware serial number
	meter_timestamp TIMESTAMP NOT NULL,
	record_type SMALLINT NOT NULL, -- use boolean or (SMALLINT)  is 2-byte signed integer that has a range from -32,768 to 32,767.
	historic_glucose NUMERIC, -- unit in mg/dL
	scan_glucose NUMERIC, -- in mg/dL
	non_numeric_rapid_acting_insulin boolean, -- indicator (flag)
	rapid_acting_insulin_unit SMALLINT, -- unit in 'unit'
	non_numeric_food boolean, --only 1 observed, as indicator (flag)
	carbohydrates_gram NUMERIC, -- unit in grams
	carbohydrates_serving SMALLINT, -- unit in servings
	non_numeric_long_acting_insulin boolean, -- indicator (flag)
	long_acting_insulin_unit SMALLINT, -- unit in 'unit'
	notes  VARCHAR (355), -- verbal description of the food
	strip_glucose NUMERIC, -- in mg/dL
	ketone NUMERIC, -- in mmol/L
	meal_insulin SMALLINT, -- unit in 'unit'
	correction_insulin SMALLINT, -- unit in 'unit'
	user_change_insulin SMALLINT -- unit in 'unit'
);

/* the table stores whatever activities a user did such as eat, sleep, run, etc. */
drop table if exists raw.activity_track;
create table raw.activity_track(
	user_id SMALLINT REFERENCES raw.user_account(user_id) ON DELETE RESTRICT,
	start_time TIMESTAMP NOT NULL, -- activity start time
	end_time TIMESTAMP, -- activity end time if for sleep
	activity_category VARCHAR(10) NOT NULL, -- activity category, e.g., food, sleep, exerise, event
	comment VARCHAR(355) -- comment about food, etc.
	-- duration numeric
);
	
