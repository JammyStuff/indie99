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
".

DownSQL = "
    DROP TABLE twitter_tokens;
".

{twitter_token,
  fun(up) -> boss_db:execute(UpSQL);
     (down) -> boss_db:execute(DownSQL)
  end}.
