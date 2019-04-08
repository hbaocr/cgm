-- SCHEMA: raw

-- DROP SCHEMA "raw" ;

CREATE SCHEMA "raw"
    AUTHORIZATION postgres;

COMMENT ON SCHEMA "raw"
    IS 'raw tables put here';
	
show search_path;
	
SET search_path TO raw,public;

/* the table stores user account information */
create table raw.user_account(
	user_id serial PRIMARY KEY,
	username VARCHAR (50) UNIQUE NOT NULL,
	password VARCHAR (50) NOT NULL,
	email VARCHAR (355) UNIQUE NOT NULL,
	created_on TIMESTAMP NOT NULL,
	last_login TIMESTAMP
);

/* the table stores cgm_librelink (continuous glucose monitoring) information */
create table raw.cgm_librelink(
	user_id serial PRIMARY KEY,
	meter_timestamp TIMESTAMP NOT NULL,
	record_type SMALLINT NOT NULL, -- (SMALLINT)  is 2-byte signed integer that has a range from -32,768 to 32,767.
	historic_glucose SMALLINT, -- unit in mg/dL
	scan_glucose SMALLINT, -- in mg/dL
	non_numeric_food boolean, --only 1 observed, as indicator (flag)
	carbohydrates_gram SMALLINT, -- unit in grams
	carbohydrates_serving SMALLINT, -- unit in servings
	notes  VARCHAR (355), -- verbal description of the food
	strip_glucose SMALLINT, -- in mg/dL
	ketone SMALLINT -- in mmol/L
)

/* the table stores whatever activities a user did such as eat, sleep, run, etc. */
create table raw.activity_track(
	user_id serial PRIMARY KEY,
	start_time TIMESTAMP NOT NULL, -- activity start time
	end_time TIMESTAMP, -- activity end time if for sleep
	activity_category VARCHAR(10) NOT NULL, -- activity category, e.g., food, sleep, exerise, event
	comment VARCHAR(355) -- comment about food, etc.
	-- duration numeric
)
	
