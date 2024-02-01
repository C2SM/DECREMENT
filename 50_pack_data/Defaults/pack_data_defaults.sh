#!/bin/bash

export PACK_COMPRESS_LEVEL=1            # compression level for ncks
export PACK_CREATE_ARCHIVE=true         # create a tarball per chunk from COSMO files; if false, original files will be replaced by compressed files
export PACK_REMOVE_ORIGINAL=true        # remove original nc files and compressed nz files after archiving COSMO files
export PACK_REMOVE_COMPRESSED=false     # remove compressed nz files (for testing)
