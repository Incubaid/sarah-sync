Example use-case of ssync
=========================

This file presents a simple use-case where we want to upload the file big.bmp, when big\_changed.bmp is already present on the system. Additionally, a third file, big\_else.bmp, which is similar to both of them, is uploaded as well. The paths of the files should be changed to the ones they are on your machine.

#### Local

To sync the files locally, we use the command

    ./ssync local "big.bmp" "big_changed.bmp" "big_out_1" "blocks" -f "auto" -d "none"

The field size will be determined automatically and no database will be used. Since this is the default behaviour, we did not need to specify these options.

#### Server-client
To simulate the server-client set-up, we first load the file big_changed.bmp into a database.

    ./ssync load_db "big_changed.bmp" "/tmp/demo.db" "blocks"

The next step is to set up the server

    ./ssync server "/tmp/demo.db" "127.0.0.1" 9000

The server is now ready to handle requests from a client. To upload big.bmp, enter

    ./ssync client "big.bmp" "big_out_2" "127.0.0.1" 9000 "blocks"

The server now disposes of the information present in both big\_changed.bmp and big.bmp and when we want to upload one of them again, no blocks should need to be send.

When we upload a third file, for instance

    ./ssync client "big_else.bmp" "big_out_3" "127.0.0.1" 9000 "blocks"
the system can use both files in the reconstruction of the new one.
