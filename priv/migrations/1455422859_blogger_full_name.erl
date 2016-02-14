%% Migration: blogger_full_name

UpSQL = "
    ALTER TABLE bloggers
        ADD COLUMN full_name VARCHAR(100);
".

DownSQL = "
    ALTER TABLE bloggers
        DROP COLUMN full_name;
".

{blogger_full_name,
  fun(up) -> boss_db:execute(UpSQL);
     (down) -> boss_db:execute(DownSQL)
  end}.
