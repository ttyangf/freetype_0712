

#include <ft2build.h>
#include FT_INTERNAL_SERVICE_H
#include FT_SERVICE_CFF_TABLE_LOAD_H

#include "psdecode.h"
#include "psobjs.h"

#include "psauxerr.h"


  /*************************************************************************/
  /*                                                                       */
  /* <Function>                                                            */
  /*    ps_decoder_init                                                    */
  /*                                                                       */
  /* <Description>                                                         */
  /*    Initializes a given glyph decoder.                                 */
  /*                                                                       */
  /* <InOut>                                                               */
  /*    decoder :: A pointer to the glyph builder to initialize.           */
  /*                                                                       */
  /* <Input>                                                               */
  /*    face      :: The current face object.                              */
  /*                                                                       */
  /*    size      :: The current size object.                              */
  /*                                                                       */
  /*    slot      :: The current glyph object.                             */
  /*                                                                       */
  /*    hinting   :: Whether hinting is active.                            */
  /*                                                                       */
  /*    hint_mode :: The hinting mode.                                     */
  /*                                                                       */
  FT_LOCAL_DEF( void )
  ps_decoder_init( PS_Decoder*     decoder,
                   TT_Face         face,
                   FT_Size         size,
                   CFF_GlyphSlot   slot,
                   FT_Byte**       glyph_names,
                   PS_Blend        blend,
                   FT_Bool         hinting,
                   FT_Render_Mode  hint_mode,
                   PS_Decoder_Get_Glyph_Callback   get_callback,
                   PS_Decoder_Free_Glyph_Callback  free_callback )
  {
    CFF_Font  cff = (CFF_Font)face->extra.data;


    /* clear everything */
    FT_ZERO( decoder );

    /* initialize builder */
    ps_builder_init( &decoder->builder, face, size, slot, hinting );

    if ( face->is_t1 )
    {
      /* retrieve PSNames interface from list of current modules */
      {
        FT_Service_PsCMaps  psnames;


        FT_FACE_FIND_GLOBAL_SERVICE( face, psnames, POSTSCRIPT_CMAPS );
        if ( !psnames )
        {
          FT_ERROR(( "ps_decoder_init:"
                     " the `psnames' module is not available\n" ));
          return FT_THROW( Unimplemented_Feature );
        }

        decoder->psnames = psnames;
      }

      /* decoder->buildchar and decoder->len_buildchar have to be  */
      /* initialized by the caller since we cannot know the length */
      /* of the BuildCharArray                                     */

      decoder->num_glyphs     = (FT_UInt)face->root.num_glyphs;
      decoder->glyph_names    = glyph_names;
      decoder->blend          = blend;
    }
    else
    {
      /* initialize Type2 decoder */
      decoder->cff          = cff;
      decoder->num_globals  = cff->global_subrs_index.count;
      decoder->globals      = cff->global_subrs;
      decoder->globals_bias = cff_compute_bias(
                                cff->top_font.font_dict.charstring_type,
                                decoder->num_globals );
    }

    decoder->hint_mode    = hint_mode;

    decoder->get_glyph_callback  = get_callback;
    decoder->free_glyph_callback = free_callback;
  }


  /* this function is used to select the subfont */
  /* and the locals subrs array                  */
  FT_LOCAL_DEF( FT_Error )
  ps_decoder_prepare( PS_Decoder*  decoder,
                      FT_Size      size,
                      FT_UInt      glyph_index )
  {
    PS_Builder   *builder = &decoder->builder;
    FT_Error      error   = FT_Err_Ok;

    if ( !builder->face->is_t1 )
    {
      CFF_Font      cff     = (CFF_Font)builder->face->extra.data;
      CFF_SubFont   sub     = &cff->top_font;

      FT_Service_CFFLoad  cffload = (FT_Service_CFFLoad)cff->cffload;

      /* manage CID fonts */
      if ( cff->num_subfonts )
      {
        FT_Byte  fd_index = cffload->fd_select_get( &cff->fd_select, glyph_index );


        if ( fd_index >= cff->num_subfonts )
        {
          FT_TRACE4(( "cff_decoder_prepare: invalid CID subfont index\n" ));
          error = FT_THROW( Invalid_File_Format );
          goto Exit;
        }

        FT_TRACE3(( "  in subfont %d:\n", fd_index ));

        sub = cff->subfonts[fd_index];

        if ( builder->hints_funcs && size )
        {
          CFF_Internal  internal = (CFF_Internal)size->internal->module_data;


          /* for CFFs without subfonts, this value has already been set */
          builder->hints_globals = (void *)internal->subfonts[fd_index];
        }

        decoder->num_locals    = sub->local_subrs_index.count;
        decoder->locals        = sub->local_subrs;
        decoder->locals_bias   = cff_compute_bias(
                                   decoder->cff->top_font.font_dict.charstring_type,
                                   decoder->num_locals );

        decoder->glyph_width   = sub->private_dict.default_width;
        decoder->nominal_width = sub->private_dict.nominal_width;

        decoder->current_subfont = sub;
      }
    }

  Exit:
    return error;
  }
