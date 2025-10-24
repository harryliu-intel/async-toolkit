// $Id$ AAG

/***************************************************************************
PNG writing utility
****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "afont.h"
#include "maplib.h"
#include "png.h"
#include <ctype.h>

void writegiffile (FILE *fp, int width, int height)
{
   png_structp png_ptr;
   png_infop   info_ptr;
   png_color_8 sig_bit;
   png_text    text[3];
   int         number_passes;
   int         i, j;
   png_bytepp  row_pointers;

   if (fp == NULL)
      return;
   
   row_pointers = (png_bytepp) map;
   row_pointers = calloc (sizeof (png_bytep), height);
   for (i = 0; i < height; i++)
   {
      row_pointers[i] = calloc (width*4,sizeof (png_bytep));
      for (j = 0; j < width; j++)
      {
	 row_pointers[i][j*4+1] = map[i][j].r;
	 row_pointers[i][j*4+2] = map[i][j].g;
	 row_pointers[i][j*4+3] = map[i][j].b;
      }
   }
   /* Create and initialize the png_struct with the desired error handler
    * functions.  If you want to use the default stderr and longjump method,
    * you can supply NULL for the last three parameters.  We also check that
    * the library version is compatible with the one used at compile time,
    * in case we are using dynamically linked libraries.  REQUIRED.
    */
   png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
      (png_voidp) NULL, NULL, NULL);

   if (png_ptr == NULL)
   {
      return;
   }

   /* Allocate/initialize the image information data.  REQUIRED */
   info_ptr = png_create_info_struct(png_ptr);
   if (info_ptr == NULL)
   {
      png_destroy_write_struct(&png_ptr,  (png_infopp)NULL);
      return;
   }

   /* Set error handling.  REQUIRED if you aren't supplying your own
    * error hadnling functions in the png_create_write_struct() call.
    */
   if (setjmp(png_ptr->jmpbuf))
   {
      /* If we get here, we had a problem */
      png_destroy_write_struct(&png_ptr,  (png_infopp)NULL);
      return;
   }

   /* set up the output control if you are using standard C streams */
   png_init_io(png_ptr, fp);

   /* Set the image information here.  Width and height are up to 2^31,
    * bit_depth is one of 1, 2, 4, 8, or 16, but valid values also depend on
    * the color_type selected. color_type is one of PNG_COLOR_TYPE_GRAY,
    * PNG_COLOR_TYPE_GRAY_ALPHA, PNG_COLOR_TYPE_PALETTE, PNG_COLOR_TYPE_RGB,
    * or PNG_COLOR_TYPE_RGB_ALPHA.  interlace is either PNG_INTERLACE_NONE or
    * PNG_INTERLACE_ADAM7, and the compression_type and filter_type MUST
    * currently be PNG_COMPRESSION_TYPE_BASE and PNG_FILTER_TYPE_BASE. REQUIRED
    */
   png_set_IHDR(png_ptr, info_ptr, width, height, 8, PNG_COLOR_TYPE_RGB,
      PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
      PNG_FILTER_TYPE_DEFAULT);

#if 0
   /* set the palette if there is one.  REQUIRED for indexed-color images */
   palette = (png_colorp)png_malloc(png_ptr, 256 * sizeof (png_color));
   /* ... set palette colors ... */
   png_set_PLTE(png_ptr, info_ptr, palette, 256);
#endif
   /* optional significant bit chunk */
   /* otherwise, if we are dealing with a color image then */
   sig_bit.red = 8;
   sig_bit.green = 8;
   sig_bit.blue = 8;
   png_set_sBIT(png_ptr, info_ptr, &sig_bit);


   /* Optionally write comments into the image */
   text[0].key = "Title";
   text[0].text = "Mona Lisa";
   text[0].compression = PNG_TEXT_COMPRESSION_NONE;
   text[1].key = "Author";
   text[1].text = "Leonardo DaVinci";
   text[1].compression = PNG_TEXT_COMPRESSION_NONE;
   text[2].key = "Description";
   text[2].text = "<long text>";
   text[2].compression = PNG_TEXT_COMPRESSION_zTXt;
   png_set_text(png_ptr, info_ptr, text, 3);

   /* Write the file header information.  REQUIRED */
   png_write_info(png_ptr, info_ptr);

   /* Once we write out the header, the compression type on the text
    * chunks gets changed to PNG_TEXT_COMPRESSION_NONE_WR or
    * PNG_TEXT_COMPRESSION_zTXt_WR, so it doesn't get written out again
    * at the end.
    */

   /* set up the transformations you want.  Note that these are
    * all optional.  Only call them if you want them.
    */

   /* invert monocrome pixels */
   png_set_invert_mono(png_ptr);

   /* Shift the pixels up to a legal bit depth and fill in
    * as appropriate to correctly scale the image.
    */
   png_set_shift(png_ptr, &sig_bit);

   /* pack pixels into bytes */
   png_set_packing(png_ptr);

   /* swap location of alpha bytes from ARGB to RGBA */
   png_set_swap_alpha(png_ptr);

   /* Get rid of filler (OR ALPHA) bytes, pack XRGB/RGBX/ARGB/RGBA into
    * RGB (4 channels -> 3 channels). The second parameter is not used.
    */
   png_set_filler(png_ptr, 0, PNG_FILLER_BEFORE);

   /* swap bytes of 16-bit files to most significant byte first */
   png_set_swap(png_ptr);

   /* swap bits of 1, 2, 4 bit packed pixel formats */
   png_set_packswap(png_ptr);
#if 0
   /* turn on interlace handling if you are not using png_write_image() */
   if (interlacing)
      number_passes = png_set_interlace_handling(png_ptr);
   else
#endif
      number_passes = 1;

   /* The easiest way to write the image (you may have a different memory
    * layout, however, so choose what fits your needs best).  You need to
    * use the first method if you aren't handling interlacing yourself.
   png_uint_32 k, height, width;
   png_byte image[height][width];
   png_bytep row_pointers[height];
   for (k = 0; k < height; k++)
     row_pointers[k] = image + k*width;

    */
   /* One of the following output methods is REQUIRED */
#if 1 /* write out the entire image data in one call */
   png_write_image(png_ptr, row_pointers);

   /* the other way to write the image - deal with interlacing */

#else /* no_entire write out the image data by one or more scanlines */
   /* The number of passes is either 1 for non-interlaced images,
    * or 7 for interlaced images.
    */
   for (pass = 0; pass < number_passes; pass++)
   {
      /* Write a few rows at a time. */
      png_write_rows(png_ptr, &row_pointers[first_row], number_of_rows);

      /* If you are only writing one row at a time, this works */
      for (y = 0; y < height; y++)
      {
         png_write_rows(png_ptr, &row_pointers[y], 1);
      }
   }
#endif /* no_entire use only one output method */

   /* It is REQUIRED to call this to finish writing the rest of the file */
   png_write_end(png_ptr, info_ptr);

#if 0
   /* if you malloced the palette, free it here */
   free(info_ptr->palette);
#endif
   /* if you allocated any text comments, free them here */

   /* clean up after the write, and free any memory allocated */
   png_destroy_write_struct(&png_ptr, (png_infopp)NULL);

   /* that's it */
   return;
}
