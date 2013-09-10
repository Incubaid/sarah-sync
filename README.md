sarah-sync
==========
In this Readme you will find information on

* [Compiling](#comp)
* [Basic usage of sarah-sync](#us)
* [Description of the program](#descr)

sarah-sync provides features to sync files, or more precisely, to construct one file based on the data present in the second.

Files are partioned into blocks, but certain blocks may already be present somewhere else on the system and can therefore be copied from that location instead of being uploaded. The information already present on the system can be represented in the form of a database, that contains references to the location where to find certain blocks. The keys in the database will be hashes of the block.

It is also possible to upload a file on a server in the same way. Only blocks that are not present yet will be sent. For blocks that are already present somewhere else, only a reference to this location is needed to find them.


<a name="comp" />Compiling
--------------------------
Compile with

    make

This will provide a command `./ssync` that can be used to:

* Load a file into a database
* Set up the server to handle requests from a client
* Upload a file on the server
* Sync two files locally, with or without database access


<a name="us" />Basic usage
----------------------------

Several command-line arguments can be provided. They are described below.
Additional information is provided by

    ./ssync --help

#### Loading a file 'file' into database 'some_db.db'

    ./ssync load_db "file" "some_db.db" "partition" -s block_size

The argument 'partition' specifies the way to partition the files into blocks. Currently 4 different partition functions are provided:

* "words": splits on the words
* "lines": splits on the lines
* "blocks": constructs blocks of specified size
* "whitespace": constucts blocks having at least the specified size, but ensures that the splits are made on whitespace

The last two functions can use the optional argument 'block_size'.

#### Setting up the server

    ./ssync server "some_db.db" "host" port -f field_size

The name of the database to use is provided, along with the IP-address and port. The optional argument 'field_size' allows the user to fix the size of the finite field that is used by the algorithm. If the argument is missing or "auto" is provided, the field will be determined by the method itself.

#### Client: upload a file on the server

    ./ssync client "file_name" "destination" "host" port "partition" -f field_size -s block_size

The file will be saved at the specified destination.

#### Local syncing

    ./ssync local "file_1" "file_2" "destination" "partition" -f field_size -s block_size -db "db_name"

The optional argument "db_name" specifies the name of the database to use. When the argument is missing or "none" is provided, no database is used.


<a name="descr" />Description
-----------------------------
The core algorithm of sarah-sync is based on set reconciliation, as described in "Set Reconciliation With Nearly Optimal Communication Complexity" by Minsky, Trachtenberg and Zippel. We are dealing with a set of blocks that need to be uploaded and we need to decide which ones are already present. To this end, the algorithm will map the blocks to elements of a finite field. The blocks present on the system are also mapped to the same field. By using the characteristic polynomials of the set, Minsky et al. present an algorithm to determine the elements from the first set that are missing in the second. These are exactly the ones we need.

The finite fields used by sarah-sync always have characteristic two. The size itself can be set by the user. If a size is not provided, the algorithm can decide for itself which field will be appropriate to use.

After it has been decided which blocks need to be uploaded, a message containing information about the construction is made. A message element can either be an entire block, since it was not present yet, or the complete hash of the block to use as a key to look up the block on the system. An additional layer of security is provided by using two different hashes to decide whether a present block definitely matches a new one.
