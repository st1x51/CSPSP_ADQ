/*
Copyright (C) 1996-1997 Id Software, Inc.
Copyright (C) 2007 Peter Mackay and Chris Swindle.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

// draw.c -- this is the only file outside the refresh that touches the
// vid buffer

#include <valarray>
#include <vector>
#include <malloc.h>
#include <pspgu.h>
#include <pspkernel.h>
#include <ctype.h>
#include <vram.h>

extern "C"
{
#include "../quakedef.h"
}

byte		*draw_chars;				// 8*8 graphic characters
qpic_t		*draw_disc;
qpic_t		*draw_backtile;

int			translate_texture;
int			char_texture;

bool 	 	tex_scale_down = true;

typedef byte texel;

typedef struct
{
	// Source.
	char	identifier[64];
	int		original_width;
	int		original_height;
	bool	stretch_to_power_of_two;
	
	// Texture description.
	int		format;
	int		filter;
	int		width;
	int		height;
	int 	mipmaps;
	int     swizzle;
	qboolean islmp;

	//Palette
	ScePspRGBA8888 *palette;
	qboolean palette_active;

	// Buffers.
	texel *ram;
	texel *vram;
	
} gltexture_t;

typedef struct
{
	int			index;	// index into gltextures[].
} glpic_t;

byte		conback_buffer[sizeof(qpic_t) + sizeof(glpic_t)];
qpic_t		*conback = (qpic_t *)&conback_buffer;
#define		NUMCROSSHAIRS 6
int			crosshairtextures[NUMCROSSHAIRS];
int			crosshairtexture_txt;
qpic_t		crosshairpic;

static byte customcrosshairdata[64];

#define CROSSHAIR_NONE	0
#define CROSSHAIR_TXT	1
#define CROSSHAIR_IMAGE	2
static int customcrosshair_loaded = CROSSHAIR_NONE;

static byte crosshairdata[NUMCROSSHAIRS][64] = {
	0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff,
	0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff,
	0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,

	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xfe, 0xfe, 0xfe, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,

	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,

	0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe,
	0xff, 0xfe, 0xff, 0xff, 0xff, 0xff, 0xfe, 0xff,
	0xff, 0xff, 0xfe, 0xff, 0xff, 0xfe, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xfe, 0xff, 0xff, 0xfe, 0xff, 0xff,
	0xff, 0xfe, 0xff, 0xff, 0xff, 0xff, 0xfe, 0xff,
	0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe,

	0xff, 0xff, 0xfe, 0xfe, 0xfe, 0xff, 0xff, 0xff,
	0xff, 0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xff,
	0xfe, 0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xfe, 0xff,
	0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xff,
	0xfe, 0xff, 0xfe, 0xfe, 0xfe, 0xff, 0xfe, 0xff,
	0xff, 0xfe, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff,
	0xff, 0xff, 0xfe, 0xfe, 0xfe, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,

	0xff, 0xff, 0xfe, 0xfe, 0xfe, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe, 0xff,
	0xfe, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xfe, 0xff,
	0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xfe, 0xfe, 0xfe, 0xff, 0xff, 0xff,
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
};
void customCrosshair_Init(void)
{
	FILE *f;
	int i = 0, c;

	customcrosshair_loaded = CROSSHAIR_NONE;

	if (FS_FOpenFile("crosshairs/crosshair.txt", &f) == -1)
		return;

	while (i < 64)
	{
		c = fgetc(f);
		if (c == EOF)
		{
			Con_Printf("Invalid format in crosshair.txt (Need 64 X's and O's)\n");
			fclose(f);
			return;
		}
		if (isspace(c))
			continue;
		if (tolower(c) != 'x' && tolower(c) != 'o')
		{
			Con_Printf("Invalid format in crosshair.txt (Only X's and O's and whitespace permitted)\n");
			fclose(f);
			return;
		}
		customcrosshairdata[i++] = (c == 'x' || c  == 'X') ? 0xfe : 0xff;
	}
	fclose(f);
	crosshairtexture_txt = GL_LoadTexture ("", 8, 8, customcrosshairdata,1, qfalse, GU_NEAREST, 0);
	customcrosshair_loaded |= CROSSHAIR_TXT;
}
#define	MAX_GLTEXTURES	1024
gltexture_t	gltextures[MAX_GLTEXTURES];
bool 		gltextures_used[MAX_GLTEXTURES];
int			numgltextures;

void GL_InitTextureUsage ()
{
	for (int i=0; i<MAX_GLTEXTURES; i++)
	{
		gltextures_used[i] = false;
	}
	numgltextures = 0;
}
//Crow_bar.
int GL_GetTexSize(int format, int w, int h)
{
   int size = 0;
   switch(format)
   {
    case GU_PSM_T4:
    case GU_PSM_DXT1:
        size = w*h/2;
		break;
	case GU_PSM_T8:
	case GU_PSM_DXT3:
	case GU_PSM_DXT5:
	    size = w*h;
        break;
	case GU_PSM_5650:
    case GU_PSM_5551:
	case GU_PSM_4444:
        size = w*h*2;
        break;
	case GU_PSM_8888:
		size = w*h*4;
        break;
   }
   return size;
}

char* GL_GetTexfName(int format)
{
   char temp[11];
   memset(temp,0,sizeof(temp));

   char *name = temp;

   switch(format)
   {
    case GU_PSM_T4:
		strcpy(temp,"GU_PSM_T4");
        break;
	case GU_PSM_T8:
        strcpy(temp,"GU_PSM_T8");
		break;
    case GU_PSM_DXT1:
        strcpy(temp,"GU_PSM_DXT1");
        break;
	case GU_PSM_DXT3:
        strcpy(temp,"GU_PSM_DXT3");
        break;
	case GU_PSM_DXT5:
        strcpy(temp,"GU_PSM_DXT5");
        break;
	case GU_PSM_5650:
        strcpy(temp,"GU_PSM_5650");
        break;
    case GU_PSM_5551:
        strcpy(temp,"GU_PSM_5551");
        break;
	case GU_PSM_4444:
        strcpy(temp,"GU_PSM_4444");
        break;
	case GU_PSM_8888:
        strcpy(temp,"GU_PSM_8888");
        break;
	default:
	    strcpy(temp,"Unknown");
   }
   return name;
}

void GL_GetTexfSize (int *w, int *h, int index)
{
     if(gltextures_used[index])
     {
	    if( w )
		   *w = gltextures[index].original_width;
	    if( h )
		   *h = gltextures[index].original_height;
	 }
}
void showimgpart (int x, int y, int px, int py, int w, int h, int texnum, int mode,unsigned int c)
{
    sceGuEnable(GU_BLEND);
   // sceGuDisable(GU_FOG); //Crow_bar. (Don't affected on 2d transformation)
    
    if (mode == 0)
	{
	   sceGuBlendFunc(GU_ADD, GU_SRC_ALPHA, GU_ONE_MINUS_SRC_ALPHA, 0, 0);
	   sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGBA);
    }
	else if (mode == 1)
	{
       sceGuDepthMask(GU_TRUE);
       sceGuBlendFunc(GU_ADD, GU_SRC_ALPHA, GU_FIX, 0, 0xFFFFFFFF);
       sceGuTexFunc(GU_TFX_MODULATE , GU_TCC_RGB);
    }
   
	GL_Bind (texnum);

	struct vertex
	{
		short u, v;
		short x, y, z;
	};

	vertex* const vertices = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2));

	vertices[0].u = px;
	vertices[0].v = py;
	vertices[0].x = x;
	vertices[0].y = y;
	vertices[0].z = 0;

	vertices[1].u = px + w;
	vertices[1].v = py + h;
	vertices[1].x = x + w;
	vertices[1].y = y + h;
	vertices[1].z = 0;
	sceGuColor(c);

	sceGuDrawArray(GU_SPRITES, GU_TEXTURE_16BIT | GU_VERTEX_16BIT | GU_TRANSFORM_2D, 2, 0, vertices);
	
	sceGuDepthMask(GU_FALSE);
	//sceGuDisable(GU_BLEND);
	sceGuColor(0xffffffff);
	//sceGuEnable(GU_FOG); //Crow_bar. (Don't affected on 2d transformation)
	sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGBA);
	sceGuBlendFunc(GU_ADD, GU_SRC_ALPHA, GU_ONE_MINUS_SRC_ALPHA, 0, 0);
}

void VID_SetPaletteTX();

void GL_Bind (int texture_index)
{
	// Binding the currently bound texture?
	if (currenttexture == texture_index)
	{
		// Don't bother.
		return;
	}

	// Remember the current texture.
	currenttexture = texture_index;

	// Which texture is it?
	const gltexture_t& texture = gltextures[texture_index];

	// Set the palette
	if(texture.format == GU_PSM_T8) //only for 8 bit textures
	{
		if(texture.palette_active == qtrue)
		{
	       // Upload the palette.
		   sceGuClutMode(GU_PSM_8888, 0, 255, 0);
           sceKernelDcacheWritebackRange(texture.palette, 256);
		   sceGuClutLoad(256 /8 , texture.palette);
		   reloaded_pallete = qfalse;
		}
		else
		{
	       if(reloaded_pallete == qfalse)
	       {
		      VID_SetPaletteTX(); //Restore old palette
		   }
		}
	}

	// Set the texture mode.
	sceGuTexMode(texture.format, texture.mipmaps , 0, texture.swizzle);
	
	if (texture.mipmaps > 0 && r_mipmaps.value > 0)
	{
		sceGuTexSlope(0.4f); // the near from 0 slope is the lower (=best detailed) mipmap it uses
		sceGuTexFilter(GU_LINEAR_MIPMAP_LINEAR, GU_LINEAR_MIPMAP_LINEAR);
		sceGuTexLevelMode(int(r_mipmaps_func.value), r_mipmaps_bias.value); // manual slope setting
	}
	else
	{
		sceGuTexFilter(texture.filter, texture.filter);
	}
	
	// Set the texture image.
	const void* const texture_memory = texture.vram ? texture.vram : texture.ram;
	sceGuTexImage(0, texture.width, texture.height, texture.width, texture_memory);
	
	
	if (texture.mipmaps > 0 && r_mipmaps.value > 0)
	{
		int size = (texture.width * texture.height);
		int offset = size;
		int div = 2;
		
		for (int i = 1; i <= texture.mipmaps; i++) {
			void* const texture_memory2 = ((byte*) texture_memory)+offset;
			sceGuTexImage(i, texture.width/div, texture.height/div, texture.width/div, texture_memory2);
			offset += size/(div*div);
			div *=2;
		}
	}
	
}

void GL_BindLM (int texture_index)
{
	// Binding the currently bound texture?
	if (currenttexture == texture_index)
	{
		// Don't bother.
		return;
	}

	// Remember the current texture.
	currenttexture = texture_index;

	// Which texture is it?
	const gltexture_t& texture = gltextures[texture_index];

	// Set the texture mode.
	sceGuTexMode(texture.format, 0, 0, GU_FALSE);
	sceGuTexFilter(texture.filter, texture.filter);

	// Set the texture image.
	const void* const texture_memory = texture.vram ? texture.vram : texture.ram;
	sceGuTexImage(0, texture.width, texture.height, texture.width, texture_memory);
}


//=============================================================================
/* Support Routines */

typedef struct cachepic_s
{
	char		name[MAX_QPATH];
	qpic_t		pic;
	byte		padding[32];	// for appended glpic
} cachepic_t;

#define	MAX_CACHED_PICS		128
cachepic_t	menu_cachepics[MAX_CACHED_PICS];
int			menu_numcachepics;

byte		menuplyr_pixels[4096];

static int GL_LoadPicTexture (qpic_t *pic)
{
	return GL_LoadTexture ("", pic->width, pic->height, pic->data, 1, qfalse, GU_NEAREST, 0);
}

qpic_t *Draw_PicFromWad (char *name)
{
	qpic_t	*p;
	glpic_t	*gl;

	p = static_cast<qpic_t*>(W_GetLumpName (name));
	gl = (glpic_t *)p->data;
	gl->index = GL_LoadPicTexture (p);

	return p;
}


/*
================
Draw_CachePic
================
*/
qpic_t	*Draw_CachePic (char *path)
{
	cachepic_t	*pic;
	int			i;
	qpic_t		*dat;
	glpic_t		*gl;
	char		str[128];

	strcpy (str, path);
	for (pic=menu_cachepics, i=0 ; i<menu_numcachepics ; pic++, i++)
		if (!strcmp (str, pic->name))
			return &pic->pic;

	if (menu_numcachepics == MAX_CACHED_PICS)
		Sys_Error ("menu_numcachepics == MAX_CACHED_PICS");
	menu_numcachepics++;
	strcpy (pic->name, str);

//
// load the pic from disk
//

	int index = loadtextureimage (str, 0, 0, qfalse, GU_LINEAR);
	if(index)
	{
		pic->pic.width  = gltextures[index].original_width;
		pic->pic.height = gltextures[index].original_height;

		gltextures[index].islmp = qfalse;
		gl = (glpic_t *)pic->pic.data;
		gl->index = index;

		return &pic->pic;
	}

	dat = (qpic_t *)COM_LoadTempFile (str);
	if (!dat)
	{
		strcat (str, ".lmp");
		dat = (qpic_t *)COM_LoadTempFile (str);
		if (!dat)
		{
			Con_Printf ("Draw_CachePic: failed to load file %s\n", str);
			return NULL;
		}
	}
	SwapPic (dat);


	pic->pic.width = dat->width;
	pic->pic.height = dat->height;

	gl = (glpic_t *)pic->pic.data;
	gl->index = GL_LoadPicTexture (dat);

	gltextures[gl->index].islmp = qtrue;
	return &pic->pic;
}


static void Draw_CharToConback (int num, byte *dest)
{
	int		row, col;
	byte	*source;
	int		drawline;
	int		x;

	row = num>>4;
	col = num&15;
	source = draw_chars + (row<<10) + (col<<3);

	drawline = 8;

	while (drawline--)
	{
		for (x=0 ; x<8 ; x++)
			if (source[x] != 255)
				dest[x] = 0x60 + source[x];
		source += 128;
		dest += 320;
	}

}

/*
===============
Draw_Init
===============
*/
void Draw_Init (void)
{
	int		i;
	qpic_t	*cb;
	byte	*dest;
	int		x, y;
	char	ver[40];
	glpic_t	*gl;
	int		start;
	byte	*ncdata;

	// load the console background and the charset
	// by hand, because we need to write the version
	// string into the background before turning
	// it into a texture
	draw_chars = static_cast<byte*>(W_GetLumpName ("conchars"));
	for (i=0 ; i<256*64 ; i++)
		if (draw_chars[i] == 0)
			draw_chars[i] = 255;	// proper transparent color

	// now turn them into textures
	char_texture = GL_LoadTexture ("charset", 128, 128, draw_chars, 1, qfalse, GU_NEAREST, 0);

	start = Hunk_LowMark();

	cb = (qpic_t *)COM_LoadTempFile ("gfx/conback.tga");	
	if (!cb)
		Sys_Error ("Couldn't load gfx/conback.lmp");
	SwapPic (cb);
/*
	// hack the version number directly into the pic
	sprintf (ver, "(gl %4.2f) %4.2f", (float)GLQUAKE_VERSION, (float)VERSION);
	dest = cb->data + 320*186 + 320 - 11 - 8*strlen(ver);
	y = strlen(ver);
	for (x=0 ; x<y ; x++)
		Draw_CharToConback (ver[x], dest+(x<<3));
*/
	conback->width = cb->width;
	conback->height = cb->height;
	ncdata = cb->data;

	gl = (glpic_t *)conback->data;
	//gl->index = GL_LoadTexture ("gfx/conback.tga", conback->width, conback->height, ncdata, 1, qfalse, GU_LINEAR, 0);
	gl->index = loadtextureimage_hud ("gfx/conback");
	//if((int)cb->data != 177319192)
	//	 Sys_Error("GTFO %i",cb->data);
	conback->width = vid.width;
	conback->height = vid.height;
    // Load the crosshair pics
	for (i = 0 ; i < NUMCROSSHAIRS ; i++)
	{
		crosshairtextures[i] = GL_LoadTexture ("", 8, 8, crosshairdata[i], 1,qtrue, GU_NEAREST, 0);
	}
    customCrosshair_Init();

	//conback->width = vid.width;
	//conback->height = vid.height;
	conback->width = gltextures[gl->index].original_width;
	conback->height = gltextures[gl->index].original_height;
	// free loaded console
	Hunk_FreeToLowMark(start);

	// save a texture slot for translated picture
	// TODO Handle translating.
	/*translate_texture = texture_extension_number++;*/

#if 0
	// save slots for scraps
	scrap_texnum = texture_extension_number;
	texture_extension_number += MAX_SCRAPS;
#endif

	//
	// get the other pics we need
	//
	draw_disc = Draw_PicFromWad ("disc");
	draw_backtile = Draw_PicFromWad ("backtile");
}



/*
================
Draw_Character

Draws one 8*8 graphics character with 0 being transparent.
It can be clipped to the top of the screen to allow the console to be
smoothly scrolled off.
================
*/
void Draw_Character (int x, int y, int num)
{
	int	row, col;

	if (num == 32)
		return;		// space

	num &= 255;
	
	if (y <= -8)
		return;			// totally off screen

	row = num>>4;
	col = num&15;

	GL_Bind (char_texture);

	struct vertex
	{
		short u, v;
		short x, y, z;
	};

	vertex* const vertices = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2));

	vertices[0].u = col * 8;
	vertices[0].v = row * 8;
	vertices[0].x = x;
	vertices[0].y = y;
	vertices[0].z = 0;

	vertices[1].u = (col + 1) * 8;
	vertices[1].v = (row + 1) * 8;
	vertices[1].x = x + 8;
	vertices[1].y = y + 8;
	vertices[1].z = 0;

	sceGuDrawArray(GU_SPRITES, GU_TEXTURE_16BIT | GU_VERTEX_16BIT | GU_TRANSFORM_2D, 2, 0, vertices);
}

/*
================
Draw_String
================
*/
void Draw_String (int x, int y, char *str)
{
	while (*str)
	{
		Draw_Character (x, y, *str);
		str++;
		x += 8;
	}
}
extern"C"
{
 #include "font.c"
}

static int fontwidthtab[256] =
{
	10, 10, 10, 10,
	10, 10, 10, 10,
	10, 10, 10, 10,
	10, 10, 10, 10,

	10, 10, 10, 10,
	10, 10, 10, 10,
	10, 10, 10, 10,
	10, 10, 10, 10,

	10,  6,  8, 10, //   ! " #
	10, 10, 10,  6, // $ % & '
	10, 10, 10, 10, // ( ) * +
	 6, 10,  6, 10, // , - . /

	10, 10, 10, 10, // 0 1 2 3
	10, 10, 10, 10, // 6 5 8 7
	10, 10,  6,  6, // 10 9 : ;
	10, 10, 10, 10, // < = > ?

	16, 10, 10, 10, // @ A B C
	10, 10, 10, 10, // D E F G
	10,  2,  8, 10, // H I J K
	 8, 6, 6, 10, // L M N O

	10, 10, 10, 10, // P Q R S
	10, 10, 10, 12, // T U V W
	10, 10, 10, 10, // X Y Z [
	10, 10,  8, 10, // \ ] ^ _

	 6,  8,  8,  8, // ` a b c
	 4,  2,  6,  8, // d e f g
	 2,  4,  2,  8, // h i j k
	 4, 8,  6,  8, // l m n o

	 8,  2,  2,  2, // p q r s
	 2,  8,  8, 12, // t u v w
	 8,  8,  8, 10, // x y z {
	 8, 10,  8, 12,  // | } ~
	 
	10, 10, 10, 10,
	10, 10, 10, 10,
	10, 10, 10, 10,
	10, 10, 10, 10,

	10, 10, 10, 10,
	10, 10, 10, 10,
	10, 10, 10, 10,
	10, 10, 10, 10,

	10,  6,  8, 10, // 
	10, 10, 10,  6, //
	10, 10, 10, 10, // 
	6, 10,  6, 10, // 

	10, 10, 10, 10, //
	10, 10, 10, 10, // 
	6, 10,  6,  6, // ё 
	10, 10, 10, 10, // 

	10, 10, 10, 8, // А Б В Г
	10, 10, 12, 10, // Д Е Ж З
	10, 10, 10, 10, // И Й К Л
	12, 10, 10, 10, // М Н О П

	10, 10, 10, 10, // Р С Т У
	12, 10, 10, 10, // Ф Х Ц Ч
	12, 12, 10, 10, // Ш Щ Ъ Ы
	10, 10, 12, 10, // Ь Э Ю Я 

	8,  8,  8,  8, // а б в г
	8,  8,  10,  8,  // д е ж з
	8,  8,  8,  8, // и й к л
	10,  8,  8,  8, // м н о п

	8,  8,  5, 8, // р с т у
	8,  8,  8, 6, // ф х ц ч
	12, 14,  6, 10, // ш щ ъ ы
	6,  8,  10, 8  // ь э ю я 
};

void Draw_FrontText(char* text, int x, int y, unsigned int color, int fw) //Crow_bar
{
	int len = (int)strlen(text);

	if(!len)
	{
		return;
	}

	// Set the texture mode. 
	sceGuTexMode(GU_PSM_8888, 0, 0, 0);
	sceGuTexImage(0, 256, 256, 256, font);
    sceGuTexFilter(GU_NEAREST, GU_NEAREST);

	sceGuShadeModel(GU_SMOOTH);

    sceGuTexFunc(GU_TFX_MODULATE, GU_TCC_RGBA);
	sceGuDepthMask(GU_TRUE);

	typedef struct
	{
		float s, t;
		unsigned int c;
		float x, y, z;
	} vertex;

	vertex* const v = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2 * len));

	int i;
	for(i = 0; i < len; i++)
	{
		unsigned char c = (unsigned char)text[i];
		
		if(c < 32)
		{
			c = 0;
		}
		//else if(c >= 128)
		//{
		//	c = 0;
		//}
		
		int tx = (c & 0x0F) << 4;
		int ty = (c & 0xF0);

		vertex* v0 = &v[i*2+0];
		vertex* v1 = &v[i*2+1];

		v0->s = (float)(tx + (fw ? ((16 - fw) >> 1) : ((16 - fontwidthtab[c]) >> 1)));
		v0->t = (float)(ty);
		v0->c = color;
		v0->x = (float)(x);
		v0->y = (float)(y);
		v0->z = 0.0f;

		v1->s = (float)(tx + 16 - (fw ? ((16 - fw) >> 1) : ((16 - fontwidthtab[c]) >> 1)));
		v1->t = (float)(ty + 16);
		v1->c = color;
		v1->x = (float)(x + (fw ? fw : fontwidthtab[c]));
		v1->y = (float)(y + 16);
		v1->z = 0.0f;

		x += (fw ? fw : fontwidthtab[c]);
	}
   	sceGuDrawArray(GU_SPRITES, GU_TEXTURE_32BITF | GU_COLOR_8888 | GU_VERTEX_32BITF | GU_TRANSFORM_2D, len * 2, 0, v);

	sceGuShadeModel(GU_SMOOTH ); //gu_flat

	sceGuDepthMask(GU_FALSE);
	sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGBA);
}

/*
=============
Draw_AlphaPic
=============
*/
static void Draw_AlphaPic (int x, int y, qpic_t *pic, float alpha)
{
	if (alpha != 1.0f)
	{
		sceGuTexFunc(GU_TFX_DECAL, GU_TCC_RGBA);
	}

	glpic_t			*gl;

#if 0
	if (scrap_dirty)
		Scrap_Upload ();
#endif
	gl = (glpic_t *)pic->data;
	GL_Bind (gl->index);

	struct vertex
	{
		short			u, v;
		short			x, y, z;
	};

	vertex* const vertices = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2));

	vertices[0].u		= 0;
	vertices[0].v		= 0;
	vertices[0].x		= x;
	vertices[0].y		= y;
	vertices[0].z		= 0;

	const gltexture_t& glt = gltextures[gl->index];
	if (gltextures[gl->index].islmp)
	{
		vertices[1].u	= glt.original_width;
		vertices[1].v	= glt.original_height;
	}
	else
	{
		vertices[1].u 	= glt.width;
		vertices[1].v 	= glt.height;
	}
	vertices[1].x		= x + pic->width;
	vertices[1].y		= y + pic->height;
	vertices[1].z		= 0;

	sceGuColor(GU_RGBA(0xff, 0xff, 0xff, static_cast<unsigned int>(alpha * 255.0f)));
	sceGuDrawArray(
		GU_SPRITES,
		GU_TEXTURE_16BIT | GU_VERTEX_16BIT | GU_TRANSFORM_2D,
		2, 0, vertices);
    sceGuColor(0xffffffff);
	if (alpha != 1.0f)
	{
		sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGBA);
	}
}


/*
=============
Draw_Pic
=============
*/
void Draw_Pic (int x, int y, qpic_t *pic)
{
	Draw_AlphaPic(x, y, pic, 1.0f);
}


/*
=============
Draw_TransPic
=============
*/
void Draw_TransPic (int x, int y, qpic_t *pic)
{
	if (x < 0 || (unsigned)(x + pic->width) > vid.width || y < 0 ||
		 (unsigned)(y + pic->height) > vid.height)
	{
		Sys_Error ("Draw_TransPic: bad coordinates");
	}
		
	Draw_Pic (x, y, pic);
}


/*
=============
Draw_TransPicTranslate

Only used for the player color selection menu
=============
*/
void Draw_TransPicTranslate (int x, int y, qpic_t *pic, byte *translation)
{
/*
	int				v, u, c;
	unsigned		trans[64*64], *dest;
	byte			*src;
	int				p;

	GL_Bind (translate_texture);

	c = pic->width * pic->height;

	dest = trans;
	for (v=0 ; v<64 ; v++, dest += 64)
	{
		src = &menuplyr_pixels[ ((v*pic->height)>>6) *pic->width];
		for (u=0 ; u<64 ; u++)
		{
			p = src[(u*pic->width)>>6];
			if (p == 255)
				dest[u] = p;
			else
				dest[u] =  d_8to24table[translation[p]];
		}
	}
	glTexImage2D (GL_TEXTURE_2D, 0, gl_alpha_format, 64, 64, 0, GL_RGBA, GL_UNSIGNED_BYTE, trans);

	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	glColor3f (1,1,1);
	glBegin (GL_QUADS);
	glTexCoord2f (0, 0);
	glVertex2f (x, y);
	glTexCoord2f (1, 0);
	glVertex2f (x+pic->width, y);
	glTexCoord2f (1, 1);
	glVertex2f (x+pic->width, y+pic->height);
	glTexCoord2f (0, 1);
	glVertex2f (x, y+pic->height);
	glEnd ();*/

	struct vertex
	{
		short u, v;
		short x, y, z;
	};

	vertex* const vertices = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2));

	vertices[0].u = 0;
	vertices[0].v = 0;
	vertices[0].x = x;
	vertices[0].y = y;
	vertices[0].z = 0;

	vertices[1].u = 1;
	vertices[1].v = 1;
	vertices[1].x = x + pic->width;
	vertices[1].y = y + pic->height;
	vertices[1].z = 0;

	sceGuDisable(GU_TEXTURE_2D);
	sceGuColor(GU_RGBA(0xff, 0, 0, 0xff));
	sceGuDrawArray(GU_SPRITES, GU_TEXTURE_16BIT | GU_VERTEX_16BIT | GU_TRANSFORM_2D, 2, 0, vertices);
	sceGuColor(0xffffffff);
	sceGuEnable(GU_TEXTURE_2D);
}


/*
================
Draw_ConsoleBackground

================
*/
void Draw_ConsoleBackground (int lines)
{
	int y = (vid.height * 3) >> 2;

	if (lines > y)
		Draw_Pic(0, lines - vid.height, conback);
	else
		Draw_AlphaPic (0, lines - vid.height, conback, (float)(1.2 * lines)/y);
}


/*
=============
Draw_TileClear

This repeats a 64*64 tile graphic to fill the screen around a sized down
refresh window.
=============
*/
void Draw_TileClear (int x, int y, int w, int h)
{
	GL_Bind (*(int *)draw_backtile->data);

	struct vertex
	{
		short u, v;
		short x, y, z;
	};

	vertex* const vertices = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2));

	vertices[0].u = x;
	vertices[0].v = y;
	vertices[0].x = x;
	vertices[0].y = y;
	vertices[0].z = 0;

	vertices[1].u = x + w;
	vertices[1].v = y + h;
	vertices[1].x = x + w;
	vertices[1].y = y + h;
	vertices[1].z = 0;

	sceGuDrawArray(GU_SPRITES, GU_TEXTURE_16BIT | GU_VERTEX_16BIT | GU_TRANSFORM_2D, 2, 0, vertices);
}


/*
=============
Draw_Fill

Fills a box of pixels with a single color
=============
*/
void Draw_Fill (int x, int y, int w, int h, int c)
{
	struct vertex
	{
		short x, y, z;
	};

	vertex* const vertices = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2));

	vertices[0].x = x;
	vertices[0].y = y;
	vertices[0].z = 0;

	vertices[1].x = x + w;
	vertices[1].y = y + h;
	vertices[1].z = 0;

	sceGuDisable(GU_TEXTURE_2D);
	sceGuColor(c);
	sceGuDrawArray(GU_SPRITES, GU_VERTEX_16BIT | GU_TRANSFORM_2D, 2, 0, vertices);
	sceGuColor(0xffffffff);
	sceGuEnable(GU_TEXTURE_2D);
}
byte *StringToRGB (char *s)
{
	byte		*col;
	static	byte	rgb[4];

	Cmd_TokenizeString (s);
	if (Cmd_Argc() == 3)
	{
		rgb[0] = (byte)Q_atoi(Cmd_Argv(0));
		rgb[1] = (byte)Q_atoi(Cmd_Argv(1));
		rgb[2] = (byte)Q_atoi(Cmd_Argv(2));
	}
	else
	{
		col = (byte *)&d_8to24table[(byte)Q_atoi(s)];
		rgb[0] = col[0];
		rgb[1] = col[1];
		rgb[2] = col[2];
	}
	rgb[3] = 255;

	return rgb;
}

extern "C"	cvar_t	crosshair, cl_crossx, cl_crossy;

cvar_t	crosshairalpha	= {"crosshairalpha", "1", qtrue};
cvar_t	crosshairsize	= {"crosshairsize",  "2",   qtrue};
cvar_t	crosshaircolor	= {"crosshaircolor", "79",  qtrue};

/*
================
Draw_Crosshair
================
*/
void Draw_Crosshair (void)
{
	float		x, y, ofs1, ofs2, sh, th, sl, tl;
	byte		*col;
	extern vrect_t	scr_vrect;
	unsigned int c,a;

	if ((crosshair.value >= 2 && crosshair.value <= NUMCROSSHAIRS + 1) ||
		((customcrosshair_loaded & CROSSHAIR_TXT) && crosshair.value == 1) ||
		(customcrosshair_loaded & CROSSHAIR_IMAGE)
	) {
		x = scr_vrect.x + scr_vrect.width / 2 + cl_crossx.value;
		y = scr_vrect.y + scr_vrect.height / 2 + cl_crossy.value;

		if (!crosshairalpha.value)
			return;

		sceGuTexFunc(GU_TFX_MODULATE , GU_TCC_RGBA);

		//col = StringToRGB (crosshaircolor.string);
		c = crosshaircolor.value;
		if (crosshairalpha.value)
		{
			a = bound(0, crosshairalpha.value, 1) * 255;
			sceGuColor(GU_RGBA(host_basepal[c*3], host_basepal[c*3+1], host_basepal[c*3+2], a));
		}
		else
		{
			sceGuColor(GU_RGBA(host_basepal[c*3], host_basepal[c*3+1], host_basepal[c*3+2], 0xff));
		}

		GL_Bind ((crosshair.value >= 2) ? crosshairtextures[(int) crosshair.value - 2] : crosshairtexture_txt);
        const gltexture_t& glt = gltextures[(crosshair.value >= 2) ? crosshairtextures[(int) crosshair.value - 2] : crosshairtexture_txt];

		ofs1 = 3.5;
		ofs2 = 4.5;
		tl = sl = 0;
		th = glt.width;
		sh = glt.height;

		ofs1 *= (vid.width / 320) * bound(0, crosshairsize.value, 20);
		ofs2 *= (vid.width / 320) * bound(0, crosshairsize.value, 20);
		
		 struct vertex
	     {
		   short u, v;
		   short x, y, z;
	     };

		 vertex* const vertices = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2));

	     vertices[0].u = tl;
	     vertices[0].v = sl;
	     vertices[0].x = x - ofs1;
	     vertices[0].y = y - ofs1;
	     vertices[0].z = 0;

	     vertices[1].u = th;
	     vertices[1].v = sh;
	     vertices[1].x = x + ofs2;
	     vertices[1].y = y + ofs2;
	     vertices[1].z = 0;


		sceGuDrawArray(GU_SPRITES, GU_TEXTURE_16BIT | GU_VERTEX_16BIT | GU_TRANSFORM_2D, 2, 0, vertices);

        sceGuTexFunc(GU_TFX_REPLACE , GU_TCC_RGBA);
		sceGuColor(0xffffffff);
	}
	else if (crosshair.value)
	{
		Draw_Character (scr_vrect.x + scr_vrect.width / 2 - 4 + cl_crossx.value, scr_vrect.y + scr_vrect.height / 2 - 4 + cl_crossy.value, '+');
	}
}
//=============================================================================

/*
================
Draw_FadeScreen

================
*/
void Draw_FadeScreen (void)
{
	struct vertex
	{
		short	x, y, z;
	};

	vertex* const vertices = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2));

	vertices[0].x		= 0;
	vertices[0].y		= 0;
	vertices[0].z		= 0;
	vertices[1].x		= vid.width;
	vertices[1].y		= vid.height;
	vertices[1].z		= 0;

	sceGuDisable(GU_TEXTURE_2D);

	sceGuColor(GU_RGBA(0, 0, 0, 0x80));
	sceGuDrawArray(
		GU_SPRITES,
		GU_VERTEX_16BIT | GU_TRANSFORM_2D,
		2, 0, vertices);
	sceGuColor(0xffffffff);

	sceGuEnable(GU_TEXTURE_2D);

	Hud_Changed();
}


/*
================
Draw_FadeScreen

================
*/
void Draw_FadeScreenColor (int r, int g, int b, int a)
{
	struct vertex
	{
		short	x, y, z;
	};

	vertex* const vertices = static_cast<vertex*>(sceGuGetMemory(sizeof(vertex) * 2));

	vertices[0].x		= 0;
	vertices[0].y		= 0;
	vertices[0].z		= 0;
	vertices[1].x		= vid.width;
	vertices[1].y		= vid.height;
	vertices[1].z		= 0;

	sceGuDisable(GU_TEXTURE_2D);

	sceGuColor(GU_RGBA(r, g, b, a));
	sceGuDrawArray(
		GU_SPRITES,
		GU_VERTEX_16BIT | GU_TRANSFORM_2D,
		2, 0, vertices);
	sceGuColor(0xffffffff);

	sceGuEnable(GU_TEXTURE_2D);

	//Sbar_Changed();
}

//=============================================================================

/*
================
Draw_BeginDisc

Draws the little blue disc in the corner of the screen.
Call before beginning any disc IO.
================
*/
void Draw_BeginDisc (void)
{
	if (!draw_disc)
		return;

	//glDrawBuffer  (GL_FRONT);
	Draw_Pic (vid.width - 24, 0, draw_disc);
	//glDrawBuffer  (GL_BACK);
}


/*
================
Draw_EndDisc

Erases the disc icon.
Call after completing any disc IO
================
*/
void Draw_EndDisc (void)
{
}

/*
================
GL_Set2D

Setup as if the screen was 320*200
================
*/
void GL_Set2D (void)
{
	sceGuViewport (glx, gly, glwidth, glheight);
	sceGuScissor(0, 0, glwidth, glheight);

	sceGuEnable(GU_BLEND);
	sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGBA);
}


static void GL_ResampleTexture(const byte *in, int inwidth, int inheight, unsigned char *out,  int outwidth, int outheight)
{
	const unsigned int fracstep = inwidth * 0x10000 / outwidth;
	for (int i = 0; i < outheight ; ++i, out += outwidth)
	{
		const byte*		inrow	= in + inwidth * (i * inheight / outheight);
		unsigned int	frac	= fracstep >> 1;
		for (int j = 0; j < outwidth; ++j, frac += fracstep)
		{
			out[j] = inrow[frac >> 16];
		}
	}
}

static void swizzle_fast(u8* out, const u8* in, unsigned int width, unsigned int height)
{
	unsigned int blockx, blocky;
	unsigned int j;

	unsigned int width_blocks = (width / 16);
	unsigned int height_blocks = (height / 8);

	unsigned int src_pitch = (width-16)/4;
	unsigned int src_row = width * 8;

	const u8* ysrc = in;
	u32* dst = (u32*)out;

	for (blocky = 0; blocky < height_blocks; ++blocky)
	{
		const u8* xsrc = ysrc;
		for (blockx = 0; blockx < width_blocks; ++blockx)
		{
			const u32* src = (u32*)xsrc;
			for (j = 0; j < 8; ++j)
			{
				*(dst++) = *(src++);
				*(dst++) = *(src++);
				*(dst++) = *(src++);
				*(dst++) = *(src++);
				src += src_pitch;
			}
			xsrc += 16;
		}
		ysrc += src_row;
	}
}

void GL_Upload8(int texture_index, const byte *data, int width, int height)
{
	if ((texture_index < 0) || (texture_index >= MAX_GLTEXTURES) || gltextures_used[texture_index] == false)
	{
		Sys_Error("Invalid texture index %d", texture_index);
	}

	const gltexture_t& texture = gltextures[texture_index];

	// Check that the texture matches.
	if ((width != texture.original_width) != (height != texture.original_height))
	{
		Sys_Error("Attempting to upload a texture which doesn't match the destination");
	}

	// Create a temporary buffer to use as a source for swizzling.
	std::size_t buffer_size = texture.width * texture.height;
	std::vector<byte> unswizzled(buffer_size);
	
	if (texture.mipmaps > 0) {
		int size_incr = buffer_size/4;
		for (int i= 1;i <= texture.mipmaps;i++) {
			buffer_size += size_incr;
			size_incr = size_incr/4;
		}
	}
	
	// Do we need to resize?
	if (texture.stretch_to_power_of_two)
	{
		// Resize.
		GL_ResampleTexture(data, width, height, &unswizzled[0], texture.width, texture.height);
	}
	else
	{
		// Straight copy.
		for (int y = 0; y < height; ++y)
		{
			const byte* const	src	= data + (y * width);
			byte* const			dst = &unswizzled[y * texture.width];
			memcpy(dst, src, width);
		}
	}
		
	// Swizzle to system RAM.
	swizzle_fast(texture.ram, &unswizzled[0], texture.width, texture.height);
	
	if (texture.mipmaps > 0) {
		int size = (texture.width * texture.height);
		int offset = size;
		int div = 2;

		for (int i = 1; i <= texture.mipmaps;i++) {
			GL_ResampleTexture(data, width, height, &unswizzled[0], texture.width/div, texture.height/div);
			swizzle_fast(texture.ram+offset, &unswizzled[0], texture.width/div, texture.height/div);
			offset += size/(div*div);
			div *=2;
		}
	}
	
	unswizzled.clear();

	// Copy to VRAM?
	if (texture.vram)
	{
		// Copy.
		memcpy(texture.vram, texture.ram, buffer_size);

		// Flush the data cache.
		sceKernelDcacheWritebackRange(texture.vram, buffer_size);
	}

	// Flush the data cache.
	sceKernelDcacheWritebackRange(texture.ram, buffer_size);
}

void GL_Upload8_A(int texture_index, const byte *data, int width, int height)
{
	if ((texture_index < 0) || (texture_index >= MAX_GLTEXTURES) || gltextures_used[texture_index] == false)
	{
		Sys_Error("Invalid texture index %d", texture_index);
	}

	const gltexture_t& texture = gltextures[texture_index];

	// Check that the texture matches.
	if ((width != texture.original_width) != (height != texture.original_height))
	{
		Sys_Error("Attempting to upload a texture which doesn't match the destination");
	}

	// Create a temporary buffer to use as a source for swizzling.
	const std::size_t buffer_size = texture.width * texture.height;
	memcpy((void *) texture.ram, (void *) data, buffer_size);
	
	
	// Copy to VRAM?
	if (texture.vram)
	{
		// Copy.
		memcpy(texture.vram, texture.ram, buffer_size);

		// Flush the data cache.
		sceKernelDcacheWritebackRange(texture.vram, buffer_size);
	}

	// Flush the data cache.
	sceKernelDcacheWritebackRange(texture.ram, buffer_size);
}

/*
================
GL_Upload16
================
*/
void GL_Upload16(int texture_index, const byte *data, int width, int height)
{
	if ((texture_index < 0) || (texture_index >= MAX_GLTEXTURES) || gltextures_used[texture_index] == false)
	{
		Sys_Error("Invalid texture index %d", texture_index);
	}

	const gltexture_t& texture = gltextures[texture_index];

	// Check that the texture matches.
	if ((width != texture.original_width) != (height != texture.original_height))
	{
		Sys_Error("Attempting to upload a texture which doesn't match the destination");
	}

	// Create a temporary buffer to use as a source for swizzling.
	const std::size_t buffer_size = texture.width * texture.height * 2;
    std::vector<byte> unswizzled(buffer_size);


	// Straight copy.
	for (int y = 0; y < height; ++y)
	{
		const byte* const	src	= data + (y * width * 2);
		byte* const			dst = &unswizzled[y * texture.width * 2];
		memcpy(dst, src, width * 2);
	}

	// Swizzle to system RAM.
	swizzle_fast(texture.ram, &unswizzled[0], texture.width * 2, texture.height);
	unswizzled.clear();


	// Copy to VRAM?
	if (texture.vram)
	{
		// Copy.
		memcpy(texture.vram, texture.ram, buffer_size);

		// Flush the data cache.
		sceKernelDcacheWritebackRange(texture.vram, buffer_size);
	}

	// Flush the data cache.
	sceKernelDcacheWritebackRange(texture.ram, buffer_size);
}

void GL_Upload16_A(int texture_index, const byte *data, int width, int height)
{
	if ((texture_index < 0) || (texture_index >= MAX_GLTEXTURES) || gltextures_used[texture_index] == false)
	{
		Sys_Error("Invalid texture index %d", texture_index);
	}

	const gltexture_t& texture = gltextures[texture_index];

	// Check that the texture matches.
	if ((width != texture.original_width) != (height != texture.original_height))
	{
		Sys_Error("Attempting to upload a texture which doesn't match the destination");
	}

	// Create a temporary buffer to use as a source for swizzling.
	const std::size_t buffer_size = texture.width * texture.height * 2;
	memcpy((void *) texture.ram, (void *) data, buffer_size);
	
	
	// Copy to VRAM?
	if (texture.vram)
	{
		// Copy.
		memcpy(texture.vram, texture.ram, buffer_size);

		// Flush the data cache.
		sceKernelDcacheWritebackRange(texture.vram, buffer_size);
	}

	// Flush the data cache.
	sceKernelDcacheWritebackRange(texture.ram, buffer_size);
}

/*
=======================================
TextureConvector
=======================================
From DE2 by Christoph Arnold "charnold"
modify by Crow_bar
=======================================
*/
void TextureConvector(unsigned char *in32, unsigned char *out16, int w, int h, int format)
{
	int texel;

	int size = w * h;

	for (texel = 0; texel < size; texel++)
	{
		if (format == GU_PSM_4444)
		{
			*(out16)    = (*in32>>4) & 0x0f; in32++; // r
			*(out16++) |= (*in32)    & 0xf0; in32++; // g
			*(out16)    = (*in32>>4) & 0x0f; in32++; // b
			*(out16++) |= (*in32)    & 0xf0; in32++; // a
		}
		else if (format == GU_PSM_5650)
		{
			unsigned char r,g,b;

			r = (*in32>>3) & 0x1f; in32++;	// r = 5 bit
			g = (*in32>>2) & 0x3f; in32++;	// g = 6 bit
			b = (*in32>>3) & 0x1f; in32++;	// b = 5 bit
								   in32++;	// a = 0 bit

			*(out16)	= r;				// alle   5 bits von r auf lower  5 bits von out16
			*(out16++) |= (g<<5) & 0xe0;	// lower  3 bits von g auf higher 3 bits von out16
			*(out16)	= (g>>3) & 0x07;	// higher 3 bits von g auf lower  3 bits von out16
			*(out16++) |= (b<<3) & 0xf8;    // alle   5 bits von b auf higher 5 bits von out16

		}
		else if (format == GU_PSM_5551)
		{
			unsigned char r,g,b,a;

			r = (*in32>>3) & 0x1f; in32++;	// r = 5 bit
			g = (*in32>>3) & 0x1f; in32++;	// g = 5 bit
			b = (*in32>>3) & 0x1f; in32++;	// b = 5 bit
			a = (*in32>>7) & 0x01; in32++;	// a = 1 bit

			*(out16)	= r;				// alle   5 bits von r auf lower  5 bits von out16
			*(out16++) |= (g<<5) & 0xe0;	// lower  3 bits von g auf higher 3 bits von out16
			*(out16)	= (g>>3) & 0x03;	// higher 2 bits von g auf lower  2 bits von out16
			*(out16)   |= (b<<2) & 0x7c;    // alle   5 bits von b auf bits 3-7      von out16
			*(out16++) |= (a<<7) & 0x80;    //        1 bit  von a auf bit    8      von out16
		}
	}
}

/*
================
GL_Upload32to16
================
*/
void GL_Upload32to16(int texture_index, const byte *data, int width, int height)
{
	if ((texture_index < 0) || (texture_index >= MAX_GLTEXTURES) || gltextures_used[texture_index] == false)
	{
		Sys_Error("Invalid texture index %d", texture_index);
	}

	const gltexture_t& texture = gltextures[texture_index];

	// Check that the texture matches.
	if ((width != texture.original_width) != (height != texture.original_height))
	{
		Sys_Error("Attempting to upload a texture which doesn't match the destination");
	}

	// Create a temporary buffer to use as a source for swizzling.
	std::size_t buffer_sizesrc = GL_GetTexSize(GU_PSM_8888, texture.width, texture.height);
    std::vector<byte> unswizzled(buffer_sizesrc);

	std::size_t buffer_sizedst = GL_GetTexSize(texture.format, texture.width, texture.height);
	std::vector<byte> bpp(buffer_sizedst);


	if (texture.stretch_to_power_of_two)
	{
	  // Resize.
	  Image_Resample ((void*)data, width, height, &unswizzled[0], texture.width, texture.height, 4, int(r_tex_res.value));
  	}
	else
	{
		// Straight copy.
		for (int y = 0; y < height; ++y)
		{
			const byte* const	src	= data + (y * width * 4);
			byte* const			dst = &unswizzled[y * texture.width * 4];
			memcpy(dst, src, width * 4);
		}
	}

	TextureConvector((unsigned char*)&unswizzled[0], (unsigned char*)&bpp[0], texture.width, texture.height, texture.format);
    unswizzled.clear();

	// Swizzle to system RAM.
	swizzle_fast(texture.ram, &bpp[0], texture.width * 2, texture.height);
    bpp.clear();


	// Copy to VRAM?
	if (texture.vram)
	{
		// Copy.
		memcpy(texture.vram, texture.ram, buffer_sizedst);

		// Flush the data cache.
		sceKernelDcacheWritebackRange(texture.vram, buffer_sizedst);
	}

	// Flush the data cache.
	sceKernelDcacheWritebackRange(texture.ram, buffer_sizedst);
}

/*
================
GL_Upload32
================
*/
void GL_Upload32(int texture_index, const byte *data, int width, int height)
{
	if ((texture_index < 0) || (texture_index >= MAX_GLTEXTURES) || gltextures_used[texture_index] == false)
	{
		Sys_Error("Invalid texture index %d", texture_index);
	}

	//const
	gltexture_t& texture = gltextures[texture_index];

	// Check that the texture matches.
	if ((width != texture.original_width) != (height != texture.original_height))
	{
		Sys_Error("Attempting to upload a texture which doesn't match the destination");
	}

	// Create a temporary buffer to use as a source for swizzling.
	std::size_t buffer_size = GL_GetTexSize(GU_PSM_8888, texture.width, texture.height);
	std::vector<byte> unswizzled(buffer_size);

	if (texture.mipmaps > 0)
	{
		int size_incr = buffer_size/4;

		for (int i= 1;i <= texture.mipmaps;i++)
		{
			buffer_size += size_incr;
			size_incr = size_incr/4;
		}
	}
	
	// Do we need to resize?
	if (texture.stretch_to_power_of_two)
	{
	   // Resize.
	   Image_Resample ((void*)data, width, height, &unswizzled[0], texture.width, texture.height, 4, int(r_tex_res.value));
  	}
	else
	{
		// Straight copy.
		for (int y = 0; y < height; ++y)
		{
			const byte* const	src	= data + (y * width * 4);
			byte* const			dst = &unswizzled[y * texture.width * 4];
			memcpy(dst, src, width * 4);
		}
	}

	// Swizzle to system RAM.
	swizzle_fast(texture.ram, &unswizzled[0], texture.width * 4, texture.height);


    if (texture.mipmaps > 0)
	{
		int size = GL_GetTexSize(GU_PSM_8888, texture.width, texture.height);
		int offset = size;
		int div = 2;

		for (int i = 1; i <= texture.mipmaps;i++)
		{
			Image_Resample((void*)data, width, height, &unswizzled[0],
			texture.width/div, texture.height/div, 4, int(r_tex_res.value));
			swizzle_fast(texture.ram+offset, &unswizzled[0], (texture.width/div) * 4, texture.height/div);
			offset += size/(div*div);
			div *=2;
		}
	}

	unswizzled.clear();

	// Copy to VRAM?
	if (texture.vram)
	{
		// Copy.
		memcpy(texture.vram, texture.ram, buffer_size);

		// Flush the data cache.
		sceKernelDcacheWritebackRange(texture.vram, buffer_size);
	}

	// Flush the data cache.
	sceKernelDcacheWritebackRange(texture.ram, buffer_size);
}

/*
================
GL_Upload32toDXT
================
*/
void GL_Upload32toDXT(int texture_index, const byte *data, int width, int height)
{
	if ((texture_index < 0) || (texture_index >= MAX_GLTEXTURES) || gltextures_used[texture_index] == false)
	{
		Sys_Error("Invalid texture index %d", texture_index);
	}

	const gltexture_t& texture = gltextures[texture_index];

	// Check that the texture matches.
	if ((width != texture.original_width) != (height != texture.original_height))
	{
		Sys_Error("Attempting to upload a texture which doesn't match the destination");
	}

	// Create a temporary buffer to use as a source for swizzling.
	std::size_t buffer_sizesrc = GL_GetTexSize(GU_PSM_8888, texture.width, texture.height);
    std::vector<byte> unswizzled(buffer_sizesrc);

    // New compressed texture
	std::size_t buffer_sizedst = GL_GetTexSize(texture.format, texture.width, texture.height);

	if (texture.stretch_to_power_of_two)
	{
	    // Resize.
	    Image_Resample ((void*)data, width, height, &unswizzled[0], texture.width, texture.height, 4, int(r_tex_res.value));
  	}
	else
	{
		// Straight copy.
		for (int y = 0; y < height; ++y)
		{
			const byte* const	src	= data + (y * width * 4);
			byte* const			dst = &unswizzled[y * texture.width * 4];
			memcpy(dst, src, width * 4);
		}
	}

	tx_compress_dxtn(4, texture.width, texture.height,(const unsigned char *)&unswizzled[0], texture.format, (unsigned char *)texture.ram);

	unswizzled.clear();

	// Copy to VRAM?
	if (texture.vram)
	{
		// Copy.
		memcpy(texture.vram, texture.ram, buffer_sizedst);

		// Flush the data cache.
		sceKernelDcacheWritebackRange(texture.vram, buffer_sizedst);
	}

	// Flush the data cache.
	sceKernelDcacheWritebackRange(texture.ram, buffer_sizedst);
}

void GL_Upload32_A(int texture_index, const byte *data, int width, int height)
{
	if ((texture_index < 0) || (texture_index >= MAX_GLTEXTURES) || gltextures_used[texture_index] == false)
	{
		Sys_Error("Invalid texture index %d", texture_index);
	}

	const gltexture_t& texture = gltextures[texture_index];

	// Check that the texture matches.
	if ((width != texture.original_width) != (height != texture.original_height))
	{
		Sys_Error("Attempting to upload a texture which doesn't match the destination");
	}

	// Create a temporary buffer to use as a source for swizzling.
	const std::size_t buffer_size = texture.width * texture.height * 4;
	memcpy((void *) texture.ram, (void *) data, buffer_size);


	// Copy to VRAM?
	if (texture.vram)
	{
		// Copy.
		memcpy(texture.vram, texture.ram, buffer_size);

		// Flush the data cache.
		sceKernelDcacheWritebackRange(texture.vram, buffer_size);
	}

	// Flush the data cache.
	sceKernelDcacheWritebackRange(texture.ram, buffer_size);
}

static std::size_t round_up(std::size_t size)
{
	static const float	denom	= 1.0f / logf(2.0f);
	const float			logged	= logf(size) * denom;
	const float			ceiling	= ceilf(logged);
	return 1 << static_cast<int>(ceiling);
}


static std::size_t round_down(std::size_t size)
{
	static const float	denom	= 1.0f / logf(2.0f);
	const float			logged	= logf(size) * denom;
	const float			floor	= floorf(logged);
	return 1 << static_cast<int>(floor);
}

void GL_UnloadTexture(int texture_index)
{
	if (gltextures_used[texture_index] == true) 
	{
		gltexture_t& texture = gltextures[texture_index];
		
		Con_DPrintf("Unloading: %s\n",texture.identifier);
		// Source.
		strcpy(texture.identifier,"");
		texture.original_width = 0;
		texture.original_height = 0;
		texture.stretch_to_power_of_two = qfalse;
		
		// Texture description.
		texture.format = GU_PSM_T8;
		texture.filter = GU_LINEAR;
		texture.width = 0;
		texture.height = 0;
		texture.mipmaps = 0;
		texture.palette_active = qfalse;
		texture.swizzle = GU_FALSE;

		// Buffers.
		if (texture.palette != NULL)
		{
            free(texture.palette);
            texture.palette = NULL;
        }
		if (texture.ram != NULL)
		{
			free(texture.ram);
			texture.ram = NULL;
		}
		if (texture.vram != NULL) 
		{
			vfree(texture.vram);
			texture.vram = NULL;
		}		
		
	}
	
	gltextures_used[texture_index] = false;
	numgltextures--;
}

int GL_LoadTexture (const char *identifier, int width, int height, const byte *data, int bpp, qboolean stretch_to_power_of_two, int filter, int mipmap_level)
{
	int texture_index = -1;
	
	tex_scale_down = r_tex_scale_down.value == qtrue;

	// See if the texture is already present.
	if (identifier[0])
	{
		for (int i = 0; i < MAX_GLTEXTURES; ++i)
		{
			if (gltextures_used[i] == true) 
			{
				const gltexture_t& texture = gltextures[i];
				if (!strcmp (identifier, texture.identifier))
				{
					return i;
				}
			}	
		}
	}

	// Out of textures?
	if (numgltextures == MAX_GLTEXTURES)
	{
		Sys_Error("Out of OpenGL textures");
	}

	// Use the next available texture.
	numgltextures++;
	texture_index = numgltextures;
		
	for (int i = 0; i < MAX_GLTEXTURES; ++i)
	{
		if (gltextures_used[i] == false)
		{
			texture_index = i;
			break;
		}
	}	
	gltexture_t& texture = gltextures[texture_index];
	gltextures_used[texture_index] = true;

	// Fill in the source data.
	strcpy(texture.identifier, identifier);
	texture.original_width			= width;
	texture.original_height			= height;
	texture.stretch_to_power_of_two	= stretch_to_power_of_two != qfalse;

	// Fill in the texture description.
	switch(bpp)
	{
	  case 1:
        texture.format		= GU_PSM_T8;
	    break;
	  case 2:
        texture.format		= GU_PSM_4444;
	    break;
	  case 4:
		switch(int(r_tex_format.value))
		{
         case 0:
		   texture.format		= GU_PSM_8888; 
		   break;
		 case 1:
           texture.format		= GU_PSM_4444;
           break;
		 case 2:
		   texture.format		= GU_PSM_DXT1;
		   break;
		 case 3:
		   texture.format		= GU_PSM_DXT3;
		   break;
		 case 4:
		   texture.format		= GU_PSM_DXT5;
		   break;
		}
		break;
	}
	
	texture.filter			= filter;
	if(texture.format < GU_PSM_DXT1)
    {
	   texture.mipmaps			= mipmap_level;
	}
	else
	{
       texture.mipmaps			= 0;
	}
	switch(texture.format)
	{
	  case GU_PSM_T8:
	  case GU_PSM_5650:
      case GU_PSM_5551:
	  case GU_PSM_4444:
	  case GU_PSM_8888:
        texture.swizzle	= GU_TRUE;
        break;
      case GU_PSM_DXT1:
      case GU_PSM_DXT3:
      case GU_PSM_DXT5:
        texture.swizzle	= GU_FALSE;
        break;
	}
	texture.palette_active  = qfalse;
	
	if (tex_scale_down == true && texture.stretch_to_power_of_two == true)
	{
		texture.width			= std::max(round_down(width), 32U);
		texture.height			= std::max(round_down(height),32U);
	}
	else
	{
		texture.width			= std::max(round_up(width), 32U);
		texture.height			= std::max(round_up(height),32U);
	}
	
	for (int i=0; i <= mipmap_level;i++)
	{
		int div = (int) powf(2,i);
		if ((texture.width / div) > 16 && (texture.height / div) > 16 )
		{
			texture.mipmaps = i;
		}
	}
	
	// Do we really need to resize the texture?
	if (texture.stretch_to_power_of_two)
	{
		// Not if the size hasn't changed.
		texture.stretch_to_power_of_two = (texture.width != width) || (texture.height != height);
	}

	Con_DPrintf("Loading(%s): %s [%dx%d](%0.2f KB)\n",
	GL_GetTexfName(texture.format),
	texture.identifier,
	texture.width,
	texture.height,
	(float) (GL_GetTexSize(texture.format, texture.width, texture.height))/1024);
	
	// Allocate the RAM.
	std::size_t buffer_size = GL_GetTexSize(texture.format, texture.width, texture.height);
	
	if (texture.mipmaps > 0)
	{
		int size_incr = buffer_size/4;
		for (int i= 1;i <= texture.mipmaps;i++)
		{
			buffer_size += size_incr;
			size_incr = size_incr/4;
		}
	}
		
	texture.ram	= static_cast<texel*>(memalign(16, buffer_size));
	
	if (!texture.ram)
	{
		Sys_Error("Out of RAM for textures.");
	}

	// Allocate the VRAM.
	texture.vram = static_cast<texel*>(valloc(buffer_size));

	// Upload the texture.
	GL_Upload8(texture_index, data, width, height);

	// Upload the texture.
	switch(bpp)
	{
	  case 1:
        GL_Upload8(texture_index, data, width, height);
        break;
	  case 2:
        GL_Upload16(texture_index, data, width, height);
        break;
	  case 4:
        switch(texture.format)
		{
          case GU_PSM_8888:
            GL_Upload32(texture_index, data, width, height);
            break;
		  case GU_PSM_5650:
          case GU_PSM_5551:
          case GU_PSM_4444:
            GL_Upload32to16(texture_index, data, width, height);
            break;
		  case GU_PSM_DXT1:
          case GU_PSM_DXT3:
          case GU_PSM_DXT5:
            GL_Upload32toDXT(texture_index, data, width, height);
            break;
		}
	}
	if (texture.vram && texture.ram)
	{
		free(texture.ram);
		texture.ram = NULL;
	}
	// Done.
	return texture_index;
}

int GL_LoadPaletteTexture (const char *identifier, int width, int height, const byte *data, byte *palette, int paltype, qboolean stretch_to_power_of_two, int filter, int mipmap_level)
{
	int texture_index = -1;

	tex_scale_down = r_tex_scale_down.value == qtrue;
	// See if the texture is already present.
	if (identifier[0])
	{
		for (int i = 0; i < MAX_GLTEXTURES; ++i)
		{
			if (gltextures_used[i] == true)
			{
				const gltexture_t& texture = gltextures[i];
				if (!strcmp (identifier, texture.identifier))
				{
					return i;
				}
			}
		}
	}

	// Out of textures?
	if (numgltextures == MAX_GLTEXTURES)
	{
		Sys_Error("Out of OpenGL textures");
	}

	// Use the next available texture.
	numgltextures++;
	texture_index = numgltextures;

	for (int i = 0; i < MAX_GLTEXTURES; ++i)
	{
		if (gltextures_used[i] == false) {
			texture_index = i;
			break;
		}
	}
	gltexture_t& texture = gltextures[texture_index];
	gltextures_used[texture_index] = true;

	// Fill in the source data.
	strcpy(texture.identifier, identifier);
	texture.original_width			= width;
	texture.original_height			= height;
	texture.stretch_to_power_of_two	= stretch_to_power_of_two != qfalse;

	// Fill in the texture description.
	texture.format			= GU_PSM_T8;
	texture.filter			= filter;
	texture.mipmaps			= mipmap_level;
	texture.palette_active  = qfalse;
	texture.swizzle	        = GU_TRUE;

	if((paltype == PAL_RGB || paltype == PAL_RGBA) && palette)
	{
        texture.palette = static_cast<ScePspRGBA8888*>(memalign(16, sizeof(ScePspRGBA8888) * 256));
		if(!texture.palette)
		{
            Sys_Error("Out of RAM for palettes.");
		}

		if(paltype == PAL_RGBA)
		{
			  // Convert the palette to PSP format.
		      for (ScePspRGBA8888* color = &texture.palette[0]; color < &texture.palette[256]; ++color)
			  {
				const unsigned int r = gammatable[*palette++];
				const unsigned int g = gammatable[*palette++];
				const unsigned int b = gammatable[*palette++];
				const unsigned int a = gammatable[*palette++];
				*color = GU_RGBA(r, g, b, a);
			  }
        }
		else if(paltype == PAL_RGB)
		{
			  // Convert the palette to PSP format.
		      for (ScePspRGBA8888* color = &texture.palette[0]; color < &texture.palette[256]; ++color)
			  {
				const unsigned int r = gammatable[*palette++];
				const unsigned int g = gammatable[*palette++];
				const unsigned int b = gammatable[*palette++];
				*color = GU_RGBA(r, g, b, 0xff);
			  }
	    }

	    if(identifier[0] == '{')
		   texture.palette[255] = 0;  //alpha color

		texture.palette_active  = qtrue;
	}
	else
	{
		Con_Printf("Warning: PaletteTexture Upload without Palette\n");
	}

	if (tex_scale_down == true && texture.stretch_to_power_of_two == true)
	{
		texture.width			= std::max(round_down(width), 32U);
		texture.height			= std::max(round_down(height),32U);
	}
	else
	{
		texture.width			= std::max(round_up(width), 32U);
		texture.height			= std::max(round_up(height),32U);
	}

	for (int i=0; i <= mipmap_level;i++)
	{
		int div = (int) powf(2,i);
		if ((texture.width / div) > 16 && (texture.height / div) > 16 )
		{
			texture.mipmaps = i;
		}
	}

	// Do we really need to resize the texture?
	if (texture.stretch_to_power_of_two)
	{
		// Not if the size hasn't changed.
		texture.stretch_to_power_of_two = (texture.width != width) || (texture.height != height);
	}

	Con_DPrintf("Loading(TUP): %s [%dx%d](%0.2f KB)\n",texture.identifier,texture.width,texture.height, (float) ((texture.width*texture.height)/1024)+(256*sizeof(ScePspRGBA8888)));

	// Allocate the RAM.
	std::size_t buffer_size = texture.width * texture.height;

	if (texture.mipmaps > 0)
	{
		int size_incr = buffer_size/4;
		for (int i= 1;i <= texture.mipmaps;i++)
		{
			buffer_size += size_incr;
			size_incr = size_incr/4;
		}
	}

	texture.ram	= static_cast<texel*>(memalign(16, buffer_size));

	if (!texture.ram)
	{
		Sys_Error("Out of RAM for textures.");
	}

	// Allocate the VRAM.
	texture.vram = static_cast<texel*>(valloc(buffer_size));

	// Upload the texture.
	GL_Upload8(texture_index, data, width, height);

	if (texture.vram && texture.ram)
	{
		free(texture.ram);
		texture.ram = NULL;
	}
	// Done.
	return texture_index;
}

int GL_LoadTextureLM (const char *identifier, int width, int height, const byte *data, int bpp, int filter, qboolean update)
{
	tex_scale_down = r_tex_scale_down.value == qtrue;
	int texture_index = -1;
	// See if the texture is already present.
	if (identifier[0])
	{
		for (int i = 0; i < MAX_GLTEXTURES; ++i)
		{
			if (gltextures_used[i] == true) 
			{
				const gltexture_t& texture = gltextures[i];
				if (!strcmp (identifier, texture.identifier))
				{
					if (update == qfalse) {
						return i;
					} 
					else {
						texture_index = i;
						break;
					}
				}
			}
		}
	}

	if (update == qfalse || texture_index == -1) {
		// Out of textures?
		if (numgltextures == MAX_GLTEXTURES)
		{
			Sys_Error("Out of OpenGL textures");
		}
	
		// Use the next available texture.
		numgltextures++;
		texture_index = numgltextures;
		
		for (int i = 0; i < MAX_GLTEXTURES; ++i)
		{
			if (gltextures_used[i] == false) {
				texture_index = i;
				break;
			}
		}	
		gltexture_t& texture = gltextures[texture_index];
		gltextures_used[texture_index] = true;
	
		// Fill in the source data.
		strcpy(texture.identifier, identifier);
		texture.original_width			= width;
		texture.original_height			= height;
		texture.stretch_to_power_of_two	= false;
	
		// Fill in the texture description.
		if (bpp == 1)
			texture.format		= GU_PSM_T8;
		else if (bpp == 2)
			texture.format		= GU_PSM_4444;
		else if (bpp == 4)
            texture.format		= GU_PSM_8888;
            
		texture.filter			= filter;
		texture.mipmaps			= 0;
		texture.swizzle	        = GU_FALSE;
		
		if (tex_scale_down == true && texture.stretch_to_power_of_two == true) {
			texture.width			= std::max(round_down(width),  16U);
			texture.height			= std::max(round_down(height), 16U);
		}
		else
		{
			texture.width			= std::max(round_up(width),  16U);
			texture.height			= std::max(round_up(height), 16U);
		}
		
		// Allocate the RAM.
		const std::size_t buffer_size = texture.width * texture.height * bpp;
		texture.ram	= static_cast<texel*>(memalign(16, buffer_size));
		if (!texture.ram)
		{
			Sys_Error("Out of RAM for lightmap textures.");
		}
	
		// Allocate the VRAM.
		//texture.vram = static_cast<texel*>(valloc(buffer_size));
	
		// Upload the texture.
		if (bpp == 1)
			GL_Upload8_A(texture_index, data, width, height);
		else if (bpp == 2)
			GL_Upload16_A(texture_index, data, width, height);
		else if (bpp == 4)
			GL_Upload32_A(texture_index, data, width, height);

		if (texture.vram && texture.ram)
		{
			free(texture.ram);
			texture.ram = NULL;
		}	
	}
	else
	{	
		gltexture_t& texture = gltextures[texture_index];
	
		if ((width == texture.original_width) && 
		    (height == texture.original_height)) {
			
				if (bpp == 1)
					GL_Upload8_A(texture_index, data, width, height);
				else if (bpp == 2)
					GL_Upload16_A(texture_index, data, width, height);
				else if (bpp == 4)
			        GL_Upload32_A(texture_index, data, width, height);
			}

		if (texture.vram && texture.ram)
		{
			free(texture.ram);
			texture.ram = NULL;
		}	
	}
	// Done.
	return texture_index;
}
