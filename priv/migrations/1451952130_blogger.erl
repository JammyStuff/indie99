%% Migration: blogger

UpSQL = "
    CREATE TABLE bloggers(
        id SERIAL,
        username VARCHAR(30) NOT NULL UNIQUE,
        password_hash VARCHAR(60) NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL,
        updated_at TIMESTAMP WITH TIME ZONE NOT NULL
    );
".

DownSQL = "
    DROP TABLE bloggers;
".

{blogger,
  fun(up) -> boss_db:execute(UpSQL);
     (down) -> boss_db:execute(DownSQL)
  end}.
