%% Migration: me_fields

UpSQL = "
    ALTER TABLE bloggers
        ADD COLUMN email_address VARCHAR(254),
        ADD COLUMN twitter_username VARCHAR(15);
".

DownSQL = "
    ALTER TABLE bloggers
        DROP COLUMN email_address,
        DROP COLUMN twitter_username;
".

{me_fields,
  fun(up) -> boss_db:execute(UpSQL);
     (down) -> boss_db:execute(DownSQL)
  end}.
