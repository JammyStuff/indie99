%% Migration: post_geolocation

UpSQL = "
    ALTER TABLE posts
        ADD COLUMN latitude DOUBLE PRECISION,
        ADD COLUMN longitude DOUBLE PRECISION;
".

DownSQL = "
    ALTER TABLE posts
        DROP COLUMN latitude,
        DROP COLUMN longitude;
".

{post_geolocation,
  fun(up) -> boss_db:execute(UpSQL);
     (down) -> boss_db:execute(DownSQL)
  end}.
