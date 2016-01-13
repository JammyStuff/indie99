%% Migration: post

UpSQL = "
    CREATE TABLE posts(
        id SERIAL PRIMARY KEY,
        title VARCHAR(116) NOT NULL,
        content TEXT,
        slug VARCHAR(150) NOT NULL UNIQUE,
        blogger_id INTEGER NOT NULL REFERENCES bloggers ON DELETE CASCADE ON UPDATE CASCADE,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL,
        updated_at TIMESTAMP WITH TIME ZONE NOT NULL
    );
".

DownSQL = "
    DROP TABLE posts;
".

{post,
  fun(up) -> boss_db:execute(UpSQL);
     (down) -> boss_db:execute(DownSQL)
  end}.
