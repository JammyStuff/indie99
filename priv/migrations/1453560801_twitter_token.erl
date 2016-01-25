%% Migration: twitter_token

UpSQL = "
    CREATE TABLE twitter_tokens(
        id SERIAL PRIMARY KEY,
        token TEXT NOT NULL,
        token_secret TEXT NOT NULL,
        blogger_id INTEGER NOT NULL UNIQUE REFERENCES bloggers ON DELETE CASCADE ON UPDATE CASCADE,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL,
        updated_at TIMESTAMP WITH TIME ZONE NOT NULL
    );
    ALTER TABLE posts
        ADD COLUMN twitter_status_id TEXT,
        ADD COLUMN twitter_username VARCHAR(15);
".

DownSQL = "
    DROP TABLE twitter_tokens;
    ALTER TABLE posts
        DROP COLUMN IF EXISTS twitter_status_id,
        DROP COLUMN IF EXISTS twitter_username;
".

{twitter_token,
  fun(up) -> boss_db:execute(UpSQL);
     (down) -> boss_db:execute(DownSQL)
  end}.
