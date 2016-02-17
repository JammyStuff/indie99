%% Migration: uploaded_file

UpSQL = "
    CREATE TABLE uploaded_files(
        id SERIAL PRIMARY KEY,
        type VARCHAR(100) NOT NULL,
        mime_type VARCHAR(100) NOT NULL,
        path VARCHAR(256),
        blogger_id INTEGER NOT NULL REFERENCES bloggers ON DELETE CASCADE ON UPDATE CASCADE,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL,
        updated_at TIMESTAMP WITH TIME ZONE NOT NULL
    );
    ALTER TABLE posts
        ADD COLUMN image_id INTEGER REFERENCES uploaded_files ON DELETE SET NULL ON UPDATE CASCADE;
".

DownSQL = "
    ALTER TABLE posts
        DROP COLUMN image_id;
    DROP TABLE uploaded_files;
".

{uploaded_file,
  fun(up) -> boss_db:execute(UpSQL);
     (down) -> boss_db:execute(DownSQL)
  end}.
