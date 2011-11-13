#include <stdio.h>
#include <memory.h>
#include <math.h>
#include <zlib.h>

typedef struct
{
  void *Memory;
  unsigned int  Size;
  unsigned int  Position;
} zglTMemory, *zglPMemory;

void zlib_Init( z_stream *strm )
{
  memset( strm, 0, sizeof( z_stream ) );
  inflateInit_( strm, ZLIB_VERSION, sizeof( z_stream ) );
}

void zlib_Free( z_stream *strm )
{
  inflateEnd( strm );
}

int png_DecodeIDAT( zglTMemory *pngMem, z_stream *pngZStream, unsigned int *pngIDATEnd, void *Buffer, int Bytes )
{
  unsigned char *b;
  char *IDATHeader;
  int result = -1;
  pngZStream->next_out  = Buffer;
  pngZStream->avail_out = Bytes;

  while ( pngZStream->avail_out > 0 )
  {
    if ( ( pngMem->Position == *pngIDATEnd ) && ( pngZStream->avail_out > 0 ) && ( pngZStream->avail_in == 0 ) )
    {
      pngMem->Position += 4;

      b = (unsigned char*)( (unsigned char*)pngMem->Memory + pngMem->Position );
      *pngIDATEnd = b[ 3 ] + ( b[ 2 ] << 8 ) + ( b[ 1 ] << 16 ) + ( b[ 0 ] << 24 );
      pngMem->Position += 4;

      IDATHeader = (char*)( (char*)pngMem->Memory + pngMem->Position );
      if ( ( IDATHeader[ 0 ] != 'I' ) && ( IDATHeader[ 1 ] != 'D' ) && ( IDATHeader[ 2 ] != 'A' ) && ( IDATHeader[ 3 ] != 'T' ) )
        return -1;
      pngMem->Position += 4;

      *pngIDATEnd += pngMem->Position;
    }

    if ( pngZStream->avail_in == 0 )
    {
      if ( pngMem->Size - pngMem->Position > 0 )
      {
        if ( pngMem->Position + 65535 > *pngIDATEnd )
        {
          if ( pngMem->Position + ( *pngIDATEnd - pngMem->Position ) > pngMem->Size )
            pngZStream->avail_in = pngMem->Size - pngMem->Position;
          else
            pngZStream->avail_in = *pngIDATEnd - pngMem->Position;
        }
        else
        {
          if ( pngMem->Position + 65535 > pngMem->Size )
            pngZStream->avail_in = pngMem->Size - pngMem->Position;
          else
            pngZStream->avail_in = 65535;
        }
        pngMem->Position += pngZStream->avail_in;
      }
      else
        pngZStream->avail_in = 0;

      if ( pngZStream->avail_in == 0 )
        return Bytes - pngZStream->avail_out;

      pngZStream->next_in = (void*)( (char*)pngMem->Memory + pngMem->Position - pngZStream->avail_in );
    }

    result = inflate( pngZStream, 0 );

    if ( result < 0 )
      return -1;
    else
      result = pngZStream->avail_in;
  }

  return result;
}
