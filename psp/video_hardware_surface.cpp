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
// r_surf.c: surface-related refresh code

#include <pspgu.h>
#include <pspgum.h>

extern "C"
{
#include "../quakedef.h"
}

#include "clipping.hpp"

using namespace quake;

int			skytexturenum;
int			last_lightmap_allocated; //ericw -- optimization: remember the index of the last lightmap AllocBlock stored a surf in

/*
#ifndef GL_RGBA4
#define	GL_RGBA4	0
#endif
*/

#define	LIGHTMAP_BYTES 4		// 1, 2, or 4
int		lightmap_textures;


#define	BLOCK_WIDTH		128
#define	BLOCK_HEIGHT	128

unsigned		blocklights[18*18*3];

int		active_lightmaps;

typedef struct glRect_s {
	unsigned char l,t,w,h;
} glRect_t;

glpoly_t	*lightmap_polys[MAX_LIGHTMAPS];
qboolean	lightmap_modified[MAX_LIGHTMAPS];
glRect_t	lightmap_rectchange[MAX_LIGHTMAPS];

int			allocated[MAX_LIGHTMAPS][BLOCK_WIDTH];

// the lightmap texture data needs to be kept in
// main memory so texsubimage can update properly
byte		lightmaps[LIGHTMAP_BYTES*MAX_LIGHTMAPS*BLOCK_WIDTH*BLOCK_HEIGHT];

int 		lightmap_index[MAX_LIGHTMAPS];

// For gl_texsort 0
msurface_t  *skychain = NULL;
msurface_t  *waterchain = NULL;

void R_RenderDynamicLightmaps (msurface_t *fa);

void 	VID_SetPaletteLM();
// switch palette for lightmaps

void 	VID_SetPaletteTX();
// switch palette for textures


/*
===============
R_AddDynamicLights
===============
*/
void R_AddDynamicLights (msurface_t *surf)
{
	int			lnum;
	int			sd, td;
	float		dist, rad, minlight;
	vec3_t		impact, local;
	int			s, t;
	int			i;
	int			smax, tmax;
	mtexinfo_t	*tex;
	// LordHavoc: .lit support begin
	float		cred, cgreen, cblue, brightness;
	unsigned	*bl;
    // LordHavoc: .lit support end

	smax = (surf->extents[0]>>4)+1;
	tmax = (surf->extents[1]>>4)+1;
	tex = surf->texinfo;

	for (lnum=0 ; lnum<MAX_DLIGHTS ; lnum++)
	{
		if ( !(surf->dlightbits & (1<<lnum) ) )
			continue;		// not lit by this light

		rad = cl_dlights[lnum].radius;
		dist = DotProduct (cl_dlights[lnum].origin, surf->plane->normal) -
				surf->plane->dist;
		rad -= fabsf(dist);
		minlight = cl_dlights[lnum].minlight;
		if (rad < minlight)
			continue;
		minlight = rad - minlight;

		for (i=0 ; i<3 ; i++)
		{
			impact[i] = cl_dlights[lnum].origin[i] -
					surf->plane->normal[i]*dist;
		}

		local[0] = DotProduct (impact, tex->vecs[0]) + tex->vecs[0][3];
		local[1] = DotProduct (impact, tex->vecs[1]) + tex->vecs[1][3];

		local[0] -= surf->texturemins[0];
		local[1] -= surf->texturemins[1];

		// LordHavoc: .lit support begin
		bl = blocklights;
		cred = cl_dlights[lnum].color[0] * 256.0f;
		cgreen = cl_dlights[lnum].color[1] * 256.0f;
		cblue = cl_dlights[lnum].color[2] * 256.0f;
        // LordHavoc: .lit support end
		
		for (t = 0 ; t<tmax ; t++)
		{
			td = ((int)local[1]) - t*16;
			if (td < 0)
				td = -td;
			for (s=0 ; s<smax ; s++)
			{
				sd = ((int)local[0]) - s*16;
				if (sd < 0)
					sd = -sd;
				if (sd > td)
					dist = sd + (td>>1);
				else
					dist = td + (sd>>1);
				if (dist < minlight)
				// LordHavoc: .lit support begin
				//	blocklights[t*smax + s] += (rad - dist)*256; // LordHavoc: original code
			    {
					brightness = rad - dist;
					bl[0] += (int) (brightness * cred);
					bl[1] += (int) (brightness * cgreen);
					bl[2] += (int) (brightness * cblue);
				}
				bl += 3;
                // LordHavoc: .lit support end
			}
		}
	}
}


/*
===============
R_BuildLightMap

Combine and scale multiple lightmaps into the 8.8 format in blocklights
===============
*/
void R_BuildLightMap (msurface_t *surf, byte *dest, int stride)
{
	int			smax, tmax;
	int			t;
	int			i, j, size;
	byte		*lightmap;
	unsigned	scale;
	int			maps;
	unsigned	*bl;

	surf->cached_dlight = (surf->dlightframe == r_framecount) ? qtrue : qfalse;

	smax = (surf->extents[0]>>4)+1;
	tmax = (surf->extents[1]>>4)+1;
	size = smax*tmax;
	lightmap = surf->samples;

// set to full bright if no light data
	if (r_fullbright.value || !cl.worldmodel->lightdata)
	{
		for (i=0 ; i<size ; i++)
		{
			blocklights[i*3+0] =
			blocklights[i*3+1] =
			blocklights[i*3+2] = 65280;
        }
		goto store;
	}

// clear to no light
	for (i=0 ; i<size ; i++)
	{
		blocklights[i*3+0] =
		blocklights[i*3+1] =
		blocklights[i*3+2] = 0;
	}

// add all the lightmaps
	if (lightmap)
	{
		for (maps = 0 ; maps < MAXLIGHTMAPS && surf->styles[maps] != 255 ; maps++)
		{
			scale = d_lightstylevalue[surf->styles[maps]];
			surf->cached_light[maps] = scale;	// 8.8 fraction

			bl = blocklights;
			for (i=0 ; i<size ; i++) // LordHavoc: original code
			{
				*bl++ += TextureToTexGamma(*lightmap++) * scale;
				*bl++ += TextureToTexGamma(*lightmap++) * scale;
				*bl++ += TextureToTexGamma(*lightmap++) * scale;
			}
		}
    }

// add all the dynamic lights
	if (surf->dlightframe == r_framecount)
		R_AddDynamicLights (surf);

// bound, invert, and shift
store:
	switch (LIGHTMAP_BYTES)
	{
	case 4:
		stride -= (smax<<2);
		bl = blocklights;
		for (i=0 ; i<tmax ; i++, dest += stride)
		{
			for (j=0 ; j<smax ; j++)
			{
				t = *bl++ >> 7;
				if (t > 255)
				    t = 255;
				*dest++ = t;

				t = *bl++ >> 7;
				if (t > 255)
				    t = 255;
				*dest++ = t;

				t = *bl++ >> 7;
				if (t > 255)
				    t = 255;
				*dest++ = t;

				*dest++ = 255;
			}
		}
		break;
	case 3:
		stride -= (smax<<2);
		bl = blocklights;
		for (i=0 ; i<tmax ; i++, dest += stride)
		{
			for (j=0 ; j<smax ; j++)
			{
				t = *bl++ >> 7;
				if (t > 255)
				    t = 255;
				*dest++ = t;
				
				t = *bl++ >> 7;
				if (t > 255)
				    t = 255;
				*dest++ = t;
				
				t = *bl++ >> 7;
				if (t > 255)
				    t = 255;
				*dest++ = t;

				*dest++ = 255;
			}
		}
		break;
	case 2:
		bl = blocklights;
		for (i=0 ; i<tmax ; i++ ,dest += stride)
		{
			for (j=0 ; j<smax ; j++)
			{
				t = ((bl[0] + bl[1] + bl[2]) * 85) >> 15; 
				bl += 3;
				
				if (t > 255)
					t = 255;
				dest[j] = t;
			}
		}
		break;
	case 1:
		bl = blocklights;
		for (i=0 ; i<tmax ; i++ ,dest += stride)
		{
			for (j=0 ; j<smax ; j++)
			{
				t = ((bl[0] + bl[1] + bl[2]) * 85) >> 15; 
				bl += 3;
				
				if (t > 255)
					t = 255;
				dest[j] = t;
			}
		}
		break;
	default:
		Sys_Error ("Bad lightmap format");
	}
}


/*
===============
R_TextureAnimation

Returns the proper texture for a given time and base texture
===============
*/
texture_t *R_TextureAnimation (texture_t *base)
{
	int		reletive;
	int		count;

	if (currententity->frame)
	{
		if (base->alternate_anims)
			base = base->alternate_anims;
	}
	
	if (!base->anim_total)
		return base;

	reletive = (int)(cl.time*10) % base->anim_total;

	count = 0;	
	while (base->anim_min > reletive || base->anim_max <= reletive)
	{
		base = base->anim_next;
		if (!base)
			Sys_Error ("R_TextureAnimation: broken cycle");
		if (++count > 100)
			Sys_Error ("R_TextureAnimation: infinite cycle");
	}

	return base;
}


/*
=============================================================

	BRUSH MODELS

=============================================================
*/


extern	int		solidskytexture;
extern	int		alphaskytexture;
extern	float	speedscale;		// for top sky and bottom sky


static inline void DrawGLPolyLM (glpoly_t *p)
{
	// Does this poly need clipped?
	

	const int				unclipped_vertex_count	= p->numverts;
	const glvert_t* const	unclipped_vertices		= &(p->verts[p->numverts]);
	
	if (clipping::is_clipping_required(
		unclipped_vertices,
		unclipped_vertex_count))
	{
		// Clip the polygon.
		const glvert_t*	clipped_vertices;
		std::size_t		clipped_vertex_count;
		clipping::clip(
			unclipped_vertices,
			unclipped_vertex_count,
			&clipped_vertices,
			&clipped_vertex_count);

		// Did we have any vertices left?
		if (clipped_vertex_count)
		{
			// Copy the vertices to the display list.
			const std::size_t buffer_size = clipped_vertex_count * sizeof(glvert_t);
			glvert_t* const display_list_vertices = static_cast<glvert_t*>(sceGuGetMemory(buffer_size));
			memcpy(display_list_vertices, clipped_vertices, buffer_size);

			// Draw the clipped vertices.
			sceGuDrawArray(
				GU_TRIANGLE_FAN,
				GU_TEXTURE_32BITF | GU_VERTEX_32BITF ,
				clipped_vertex_count, 0, display_list_vertices);
		}
	}
	else
	{
	
		// Draw the poly directly.
		sceGuDrawArray(
			GU_TRIANGLE_FAN,
			GU_TEXTURE_32BITF | GU_VERTEX_32BITF ,
			unclipped_vertex_count, 0, unclipped_vertices);
	}
}

static inline void DrawGLPoly (glpoly_t *p)
{
	// Does this poly need clipped?
	const int				unclipped_vertex_count	= p->numverts;
	const glvert_t* const	unclipped_vertices		= p->verts;
	if (clipping::is_clipping_required(
		unclipped_vertices,
		unclipped_vertex_count))
	{
		// Clip the polygon.
		const glvert_t*	clipped_vertices;
		std::size_t		clipped_vertex_count;
		clipping::clip(
			unclipped_vertices,
			unclipped_vertex_count,
			&clipped_vertices,
			&clipped_vertex_count);

		// Did we have any vertices left?
		if (clipped_vertex_count)
		{
			// Copy the vertices to the display list.
			const std::size_t buffer_size = clipped_vertex_count * sizeof(glvert_t);
			glvert_t* const display_list_vertices = static_cast<glvert_t*>(sceGuGetMemory(buffer_size));
			memcpy(display_list_vertices, clipped_vertices, buffer_size);

			// Draw the clipped vertices.
			sceGuDrawArray(
				GU_TRIANGLE_FAN,
				GU_TEXTURE_32BITF | GU_VERTEX_32BITF,
				clipped_vertex_count, 0, display_list_vertices);
		}
	}
	else
	{
		// Draw the poly directly.
		sceGuDrawArray(
			GU_TRIANGLE_FAN,
			GU_TEXTURE_32BITF | GU_VERTEX_32BITF,
			unclipped_vertex_count, 0, unclipped_vertices);
	}
}

static inline void DrawTrisPoly (glpoly_t *p) //Crow_bar
{
    sceGuDisable(GU_TEXTURE_2D);
	// Does this poly need clipped?
	const int				unclipped_vertex_count	= p->numverts;
	const glvert_t* const	unclipped_vertices		= p->verts;
	if (clipping::is_clipping_required(
		unclipped_vertices,
		unclipped_vertex_count))
	{
		// Clip the polygon.
		const glvert_t*	clipped_vertices;
		std::size_t		clipped_vertex_count;
		clipping::clip(
			unclipped_vertices,
			unclipped_vertex_count,
			&clipped_vertices,
			&clipped_vertex_count);

		// Did we have any vertices left?
		if (clipped_vertex_count)
		{
			// Copy the vertices to the display list.
			const std::size_t buffer_size = clipped_vertex_count * sizeof(glvert_t);
			glvert_t* const display_list_vertices = static_cast<glvert_t*>(sceGuGetMemory(buffer_size));
			memcpy(display_list_vertices, clipped_vertices, buffer_size);

			// Draw the clipped vertices.
			sceGuDrawArray(
				GU_LINE_STRIP,
				GU_TEXTURE_32BITF | GU_VERTEX_32BITF,
				clipped_vertex_count, 0, display_list_vertices);
		}
	}
	else
	{
		// Draw the poly directly.
		sceGuDrawArray(
			GU_LINE_STRIP,
			GU_TEXTURE_32BITF | GU_VERTEX_32BITF,
			unclipped_vertex_count, 0, unclipped_vertices);
	}
    sceGuEnable(GU_TEXTURE_2D);
}

static void DrawGLWaterPoly (glpoly_t *p)
{
#if 0
	int		i;
	const glvert_t	*v;
	vec3_t	nv;

	/*GL_DisableMultitexture();

	glBegin (GL_TRIANGLE_FAN);*/
	v = p->verts;
	for (i=0 ; i<p->numverts ; i++, ++v)
	{
		/*glTexCoord2f (v[3], v[4]);*/

		nv[0] = v->xyz[0] + 8*sinf(v->xyz[1]*0.05+realtime)*sinf(v->xyz[2]*0.05+realtime);
		nv[1] = v->xyz[1] + 8*sinf(v->xyz[0]*0.05+realtime)*sinf(v->xyz[2]*0.05+realtime);
		nv[2] = v->xyz[2];

		/*glVertex3fv (nv);*/
	}
	/*glEnd ();*/
#else
	DrawGLPoly(p);
#endif
}

#if 0
static void DrawGLWaterPolyLightmap (glpoly_t *p)
{

	int		i;
	const glvert_t	*v;
	vec3_t	nv;

	/*GL_DisableMultitexture();

	glBegin (GL_TRIANGLE_FAN);*/
	v = p->verts;
	for (i=0 ; i<p->numverts ; i++, ++v)
	{
		/*glTexCoord2f (v[5], v[6]);*/

		nv[0] = v->xyz[0] + 8*sinf(v->xyz[1]*0.05+realtime)*sinf(v->xyz[2]*0.05+realtime);
		nv[1] = v->xyz[1] + 8*sinf(v->xyz[0]*0.05+realtime)*sinf(v->xyz[2]*0.05+realtime);
		nv[2] = v->xyz[2];

		/*glVertex3fv (nv);*/
	}
	/*glEnd ();*/
}
#endif

/*
================
R_BlendLightmaps
================
*/
static void R_BlendLightmaps (void)
{
	int			 i;
	glpoly_t	*p;

	if (r_fullbright.value)
		return;

	sceGuDepthMask(GU_TRUE);
		
	sceGuEnable(GU_BLEND);
	sceGuTexFunc(GU_TFX_REPLACE , GU_TCC_RGB);
	sceGuBlendFunc(GU_ADD, GU_DST_COLOR, GU_SRC_COLOR, 0, 0);

	if (LIGHTMAP_BYTES == 1)	
		VID_SetPaletteLM();	

	if (r_lightmap.value)
	{
		sceGuDisable(GU_BLEND);
	}

	for (i=0 ; i<MAX_LIGHTMAPS ; i++)
	{
		p = lightmap_polys[i];
		if (!p)
			continue;
		
		char lm_name[16];
		if (lightmap_modified[i])
		{
			lightmap_modified[i] = qfalse;
			lightmap_rectchange[i].l = BLOCK_WIDTH;
			lightmap_rectchange[i].t = BLOCK_HEIGHT;
			lightmap_rectchange[i].w = 0;
			lightmap_rectchange[i].h = 0;
			
			sprintf(lm_name,"lightmap%d",i);
			lightmap_index[i] = GL_LoadTextureLM (lm_name, BLOCK_WIDTH, BLOCK_HEIGHT, lightmaps+(i*BLOCK_WIDTH*BLOCK_HEIGHT*LIGHTMAP_BYTES), LIGHTMAP_BYTES, GU_LINEAR, qtrue);
		}
		GL_BindLM (lightmap_index[i]);
		for ( ; p ; p=p->chain)
		{
			if (p->flags & SURF_UNDERWATER) {
				DrawGLPolyLM(p);
				//DrawGLWaterPolyLightmap (p);
			}
			else 
			{
				DrawGLPolyLM(p);
			}
		}
	}
	
	if (LIGHTMAP_BYTES == 1)	
		VID_SetPaletteTX();
	
	sceGuDisable(GU_BLEND);
	sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGB);
	sceGuBlendFunc(GU_ADD, GU_SRC_ALPHA, GU_ONE_MINUS_SRC_ALPHA, 0, 0);

	sceGuDepthMask (GU_FALSE);
	
}

/*
================
R_RenderBrushPoly
================
*/
void R_RenderBrushPoly (msurface_t *fa)
{
	texture_t	*t;
	byte		*base;
	int			maps;
	glRect_t    *theRect;
	int smax, tmax;

	c_brush_polys++;
	
	if(r_showtris.value) //Crow_bar
	{
		sceGuDepthMask (GU_TRUE);

		DrawTrisPoly (fa->polys);

        sceGuDepthMask (GU_FALSE);
	}
	if (fa->flags & SURF_DRAWSKY)
	{	// warp texture, no lightmaps
		EmitBothSkyLayers (fa);
		return;
	}
		
	t = R_TextureAnimation (fa->texinfo->texture);
	GL_Bind (t->gl_texturenum);

	if (fa->flags & SURF_DRAWTURB)
	{	// warp texture, no lightmaps
		EmitWaterPolys (fa);
		return;
	}

	if( fa->flags & SURF_CONVEYOR )
		EmitScrollPolys (fa);
	else
	{
		if (fa->flags & SURF_UNDERWATER)
			DrawGLWaterPoly (fa->polys);
		else
			DrawGLPoly (fa->polys);
    }
//Crow_bar Decals
	DrawSurfaceDecals(fa);
	
	if( fa->flags & SURF_DRAWTILED )
		return; // no lightmaps anyway

	// add the poly to the proper lightmap chain

	fa->polys->chain = lightmap_polys[fa->lightmaptexturenum];
	lightmap_polys[fa->lightmaptexturenum] = fa->polys;

	// check for lightmap modification
	for (maps = 0 ; maps < MAXLIGHTMAPS && fa->styles[maps] != 255 ;
		 maps++)
		if (d_lightstylevalue[fa->styles[maps]] != fa->cached_light[maps])
			goto dynamic;

	if (fa->dlightframe == r_framecount	// dynamic this frame
		|| fa->cached_dlight)			// dynamic previously
	{
dynamic:
		if (r_dynamic.value)
		{
			lightmap_modified[fa->lightmaptexturenum] = qtrue;
			theRect = &lightmap_rectchange[fa->lightmaptexturenum];
			if (fa->light_t < theRect->t) {
				if (theRect->h)
					theRect->h += theRect->t - fa->light_t;
				theRect->t = fa->light_t;
			}
			if (fa->light_s < theRect->l) {
				if (theRect->w)
					theRect->w += theRect->l - fa->light_s;
				theRect->l = fa->light_s;
			}
			smax = (fa->extents[0]>>4)+1;
			tmax = (fa->extents[1]>>4)+1;
			if ((theRect->w + theRect->l) < (fa->light_s + smax))
				theRect->w = (fa->light_s-theRect->l)+smax;
			if ((theRect->h + theRect->t) < (fa->light_t + tmax))
				theRect->h = (fa->light_t-theRect->t)+tmax;
			base = lightmaps + fa->lightmaptexturenum*LIGHTMAP_BYTES*BLOCK_WIDTH*BLOCK_HEIGHT;
			base += fa->light_t * BLOCK_WIDTH * LIGHTMAP_BYTES + fa->light_s * LIGHTMAP_BYTES;
			R_BuildLightMap (fa, base, BLOCK_WIDTH*LIGHTMAP_BYTES);
		}
	}
}

/*
================
R_RenderDynamicLightmaps
Multitexture
================
*/
void R_RenderDynamicLightmaps (msurface_t *fa)
{
	byte		*base;
	int			maps;
	glRect_t    *theRect;
	int smax, tmax;

	c_brush_polys++;

	if( fa->flags & SURF_DRAWTILED )
		return;
		
	fa->polys->chain = lightmap_polys[fa->lightmaptexturenum];
	lightmap_polys[fa->lightmaptexturenum] = fa->polys;

	// check for lightmap modification
	for (maps = 0 ; maps < MAXLIGHTMAPS && fa->styles[maps] != 255 ;
		 maps++)
		if (d_lightstylevalue[fa->styles[maps]] != fa->cached_light[maps])
			goto dynamic;

	if (fa->dlightframe == r_framecount	// dynamic this frame
		|| fa->cached_dlight)			// dynamic previously
	{
dynamic:
		if (r_dynamic.value)
		{
			lightmap_modified[fa->lightmaptexturenum] = qtrue;
			theRect = &lightmap_rectchange[fa->lightmaptexturenum];
			if (fa->light_t < theRect->t) {
				if (theRect->h)
					theRect->h += theRect->t - fa->light_t;
				theRect->t = fa->light_t;
			}
			if (fa->light_s < theRect->l) {
				if (theRect->w)
					theRect->w += theRect->l - fa->light_s;
				theRect->l = fa->light_s;
			}
			smax = (fa->extents[0]>>4)+1;
			tmax = (fa->extents[1]>>4)+1;
			if ((theRect->w + theRect->l) < (fa->light_s + smax))
				theRect->w = (fa->light_s-theRect->l)+smax;
			if ((theRect->h + theRect->t) < (fa->light_t + tmax))
				theRect->h = (fa->light_t-theRect->t)+tmax;
			base = lightmaps + fa->lightmaptexturenum*LIGHTMAP_BYTES*BLOCK_WIDTH*BLOCK_HEIGHT;
			base += fa->light_t * BLOCK_WIDTH * LIGHTMAP_BYTES + fa->light_s * LIGHTMAP_BYTES;
			R_BuildLightMap (fa, base, BLOCK_WIDTH*LIGHTMAP_BYTES);
		}
	}
}

/*
================
R_MirrorChain
================
*/
void R_MirrorChain (msurface_t *s)
{
	if (mirror)
		return;
	mirror = qtrue;
	mirror_plane = s->plane;
}


/*
================
R_DrawWaterSurfaces
================
*/
void R_DrawWaterSurfaces (void)
{
	int			i;
	msurface_t	*s;
	texture_t	*t;

	if (r_wateralpha.value == 1.0 /*&& gl_texsort.value*/)
		return;
	
	float alpha1 = r_wateralpha.value;
	float alpha2 = 1 - r_wateralpha.value;
	
	//
	// go back to the world matrix
	//

    /*glLoadMatrixf (r_world_matrix);*/
	sceGumMatrixMode(GU_VIEW);
	sceGumLoadMatrix(&r_world_matrix);
	sceGumUpdateMatrix();

	sceGumMatrixMode(GU_MODEL);

	if (r_wateralpha.value < 1.0) {
		sceGuEnable (GU_BLEND);
		sceGuTexFunc(GU_TFX_REPLACE , GU_TCC_RGB);
		//sceGuBlendFunc(GU_ADD, GU_ONE_MINUS_DST_ALPHA, GU_ONE_MINUS_DST_ALPHA, 0, 0);
		sceGuBlendFunc(GU_ADD, GU_FIX, GU_FIX, GU_COLOR(alpha1,alpha1,alpha1,alpha1), GU_COLOR(alpha2,alpha2,alpha2,alpha2));
		/*glEnable (GL_BLEND);
		glColor4f (1,1,1,r_wateralpha.value);
		glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);*/
	}

	/*if (!gl_texsort.value) {
		if (!waterchain)
			return;

		for ( s = waterchain ; s ; s=s->texturechain) {
			GL_Bind (s->texinfo->texture->gl_texturenum);
			EmitWaterPolys (s);
		}
		
		waterchain = NULL;
	} else*/
	{

		for (i=0 ; i<cl.worldmodel->numtextures ; i++)
		{
			t = cl.worldmodel->textures[i];
			if (!t)
				continue;
			s = t->texturechain;
			if (!s)
				continue;
			if ( !(s->flags & SURF_DRAWTURB ) )
				continue;

			// set modulate mode explicitly
			
			GL_Bind (t->gl_texturenum);

			for ( ; s ; s=s->texturechain)
				EmitWaterPolys (s);
			
			t->texturechain = NULL;
		}

	}

	if (r_wateralpha.value < 1.0) {
		sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGB);
		sceGuBlendFunc(GU_ADD, GU_SRC_ALPHA, GU_ONE_MINUS_SRC_ALPHA, 0, 0);
		sceGuColor (GU_RGBA(0xff, 0xff, 0xff, 0xff));
		sceGuDisable (GU_BLEND);
		
		/*glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

		glColor4f (1,1,1,1);
		glDisable (GL_BLEND);*/
	}

}

/*
================
DrawTextureChains
================
*/
static void DrawTextureChains (void)
{
	int		i;
	msurface_t	*s;
	texture_t	*t;
/*
	if (!gl_texsort.value) {
		GL_DisableMultitexture();

		if (skychain) {
			R_DrawSkyChain(skychain);
			skychain = NULL;
		}

		return;
	} 
*/
	for (i=0 ; i<cl.worldmodel->numtextures ; i++)
	{
		t = cl.worldmodel->textures[i];
		if (!t)
			continue;
		s = t->texturechain;
		if (!s)
			continue;
		if (i == skytexturenum)
			R_DrawSkyChain (s);
		else if (i == mirrortexturenum && r_mirroralpha.value != 1.0)
		{
			R_MirrorChain (s);
			continue;
		}
		else
		{
			if ((s->flags & SURF_DRAWTURB) && r_wateralpha.value != 1.0)
				continue;	// draw translucent water later
			for ( ; s ; s=s->texturechain)
				R_RenderBrushPoly (s);
		}

		t->texturechain = NULL;
	}
}

/*
=================
R_DrawBrushModel
=================
*/
void R_DrawBrushModel (entity_t *e)
{
	int			i, k;
	vec3_t		mins, maxs;
	msurface_t	*psurf;
	float		dot;
	mplane_t	*pplane;
	model_t		*clmodel;
	qboolean	rotated;
	int		old_rendermode;

	currententity = e;
	currenttexture = -1;

	clmodel = e->model;

	if (e->angles[0] || e->angles[1] || e->angles[2])
	{
		rotated = qtrue;
		for (i=0 ; i<3 ; i++)
		{
			mins[i] = (e->origin[i] - clmodel->radius);
			maxs[i] = (e->origin[i] + clmodel->radius);
		}
	}
	else
	{
		rotated = qfalse;
		VectorAdd (e->origin, clmodel->mins, mins);
		VectorAdd (e->origin, clmodel->maxs, maxs);
	}

	if (R_CullBox (mins, maxs))
		return;
	old_rendermode = e->rendermode;
	memset (lightmap_polys, 0, sizeof(lightmap_polys));

	VectorSubtract (r_refdef.vieworg, e->origin, modelorg);

	if (rotated)
	{
		vec3_t	temp;
		vec3_t	forward, right, up;

		VectorCopy (modelorg, temp);
		AngleVectors (e->angles, forward, right, up);
		modelorg[0] = DotProduct (temp, forward);
		modelorg[1] = -DotProduct (temp, right);
		modelorg[2] = DotProduct (temp, up);
	}

	psurf = &clmodel->surfaces[clmodel->firstmodelsurface];

// calculate dynamic lighting for bmodel if it's not an
// instanced model
	if (clmodel->firstmodelsurface != 0/* && !gl_flashblend.value*/)
	{
		for (k=0 ; k<MAX_DLIGHTS ; k++)
		{
			if ((cl_dlights[k].die < cl.time) ||
				(!cl_dlights[k].radius))
				continue;

			R_MarkLights (&cl_dlights[k], 1<<k,	clmodel->nodes + clmodel->hulls[0].firstclipnode);
		}
	}

	sceGumPushMatrix();

	//Crow_bar half_life render.
	if (e->rendermode == kRenderTransAdd)
	{
		float deg = e->renderamt / 255.0f;
		float alpha1 = deg;
	    float alpha2 = 1 - deg;

		if(deg <= 0.7)
           sceGuDepthMask(GU_TRUE);

		sceGuEnable (GU_BLEND);
		sceGuTexFunc(GU_TFX_MODULATE, GU_TCC_RGBA);
        sceGuBlendFunc(GU_ADD, GU_FIX, GU_FIX,
		GU_COLOR(alpha1,alpha1,alpha1,alpha1),
		GU_COLOR(alpha2,alpha2,alpha2,alpha2));
	}
    else if (e->rendermode == kRenderTransAlpha)
    {
		sceGuEnable(GU_ALPHA_TEST);
		sceGuTexFunc(GU_TFX_MODULATE, GU_TCC_RGBA);
	}
	else if (e->rendermode == kRenderGlow)
	{
        sceGuEnable(GU_BLEND);
		sceGuTexFunc(GU_TFX_MODULATE, GU_TCC_RGBA);
        sceGuColor(GU_RGBA(255, 255, 255, int(e->renderamt)));
    }
    else if (e->rendermode == kRenderTransTexture)
	{
		float deg =  e->renderamt / 255.0f;
		float alpha1 = deg;
	    float alpha2 = 1 - deg;
	    if(deg <= 0.7)
			sceGuDepthMask(GU_TRUE);
        sceGuEnable(GU_BLEND);
		sceGuBlendFunc(GU_ADD, GU_FIX, GU_FIX, GU_COLOR(alpha1,alpha1,alpha1,alpha1), GU_COLOR(alpha2,alpha2,alpha2,alpha2));
		sceGuTexFunc(GU_TFX_MODULATE, GU_TCC_RGBA);
		sceGuColor(GU_RGBA(255, 255, 255, int(e->renderamt))); 

    }
	else if (e->rendermode == kRenderTransColor)
	{
		sceGuTexFunc(GU_TFX_MODULATE, GU_TCC_RGBA);
		sceGuColor(GU_RGBA(int(e->rendercolor[0]), int(e->rendercolor[1]), int(e->rendercolor[2]), int(e->renderamt)));
   }

	e->angles[0] = -e->angles[0];	// stupid quake bug

    R_BlendedRotateForEntity  (e, 0);  //blend transform

	clipping::begin_brush_model();

	e->angles[0] = -e->angles[0];	// stupid quake bug
	

	//
	// draw texture
	//

	for (i=0 ; i<clmodel->nummodelsurfaces ; i++, psurf++)
	{
	// find which side of the node we are on
		pplane = psurf->plane;

		dot = DotProduct (modelorg, pplane->normal) - pplane->dist;

	// draw the polygon
		if (((psurf->flags & SURF_PLANEBACK) && (dot < -BACKFACE_EPSILON)) ||
			(!(psurf->flags & SURF_PLANEBACK) && (dot > BACKFACE_EPSILON)))
		{
				R_RenderBrushPoly (psurf);
		}
	}
	
	if (e->rendermode != kRenderTransAdd )
    {
        if(e->rendermode == kRenderTransAlpha)
        {
		  sceGuDepthFunc( GU_EQUAL );
        }
		if(e->rendermode != kRenderTransTexture)
			R_BlendLightmaps ();

		if(e->rendermode == kRenderTransAlpha)
        {
		  sceGuDepthFunc( GU_LEQUAL );
        }
	}
	
	if (e->rendermode == kRenderTransAdd)
	{
        float deg = e->renderamt / 255.0f;

		if(deg <= 0.7)
           sceGuDepthMask(GU_FALSE);

		sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGB);
		sceGuBlendFunc(GU_ADD, GU_SRC_ALPHA, GU_ONE_MINUS_SRC_ALPHA, 0, 0);
        sceGuDisable (GU_BLEND);
	}
	else if(e->rendermode == kRenderTransAlpha)
	{
        sceGuAlphaFunc(GU_GREATER, 0, 0xff);
        sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGB);
     	sceGuDisable(GU_ALPHA_TEST);
	}
    else if(e->rendermode == kRenderGlow)
    {
        sceGuColor(0xffffffff);
        sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGB);
        sceGuDisable (GU_BLEND);
    }
    else if(e->rendermode == kRenderTransTexture)
    {
        sceGuColor(0xffffffff);
        sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGB);
        sceGuDisable (GU_BLEND);
		sceGuDepthMask(GU_FALSE);
		sceGuBlendFunc(GU_ADD, GU_SRC_ALPHA, GU_ONE_MINUS_SRC_ALPHA, 0, 0);
    }
    else if(e->rendermode == kRenderTransColor)
    {
        sceGuColor(0xffffffff);
        sceGuTexFunc(GU_TFX_REPLACE, GU_TCC_RGB);
    }
	e->rendermode = old_rendermode;
	clipping::end_brush_model();

	sceGumPopMatrix();
	sceGumUpdateMatrix();
}

/*
=============================================================

	WORLD MODEL

=============================================================
*/

/*
================
R_RecursiveWorldNode
================
*/
void R_RecursiveWorldNode (mnode_t *node)
{
	int			c, side;
	mplane_t	*plane;
	msurface_t	*surf, **mark;
	mleaf_t		*pleaf;
	float		dot;

	if (node->contents == CONTENTS_SOLID)
		return;		// solid

	if (node->visframe != r_visframecount)
		return;
	if (R_CullBox (node->minmaxs, node->minmaxs+3))
		return;
	
// if a leaf node, draw stuff
	if (node->contents < 0)
	{
		pleaf = (mleaf_t *)node;

		mark = pleaf->firstmarksurface;
		c = pleaf->nummarksurfaces;

		if (c)
		{
			do
			{
				(*mark)->visframe = r_framecount;
				mark++;
			} while (--c);
		}

	// deal with model fragments in this leaf
		if (pleaf->efrags)
			R_StoreEfrags (&pleaf->efrags);

		return;
	}

// node is just a decision point, so go down the apropriate sides

// find which side of the node we are on
	plane = node->plane;

	switch (plane->type)
	{
	case PLANE_X:
		dot = modelorg[0] - plane->dist;
		break;
	case PLANE_Y:
		dot = modelorg[1] - plane->dist;
		break;
	case PLANE_Z:
		dot = modelorg[2] - plane->dist;
		break;
	default:
		dot = DotProduct (modelorg, plane->normal) - plane->dist;
		break;
	}

	if (dot >= 0)
		side = 0;
	else
		side = 1;

// recurse down the children, front side first
	R_RecursiveWorldNode (node->children[side]);

// draw stuff
	c = node->numsurfaces;

	if (c)
	{
		surf = cl.worldmodel->surfaces + node->firstsurface;

		if (dot < 0 -BACKFACE_EPSILON)
			side = SURF_PLANEBACK;
		else if (dot > BACKFACE_EPSILON)
			side = 0;
		{
			for ( ; c ; c--, surf++)
			{
				if (surf->visframe != r_framecount)
					continue;

				// don't backface underwater surfaces, because they warp
				if ( !(surf->flags & SURF_UNDERWATER) && ( (dot < 0) ^ !!(surf->flags & SURF_PLANEBACK)) )
					continue;		// wrong side

				// if sorting by texture, just store it out
				/*if (gl_texsort.value)*/
				{
					if (!mirror
					|| surf->texinfo->texture != cl.worldmodel->textures[mirrortexturenum])
					{
						surf->texturechain = surf->texinfo->texture->texturechain;
						surf->texinfo->texture->texturechain = surf;
					}
				}/* else if (surf->flags & SURF_DRAWSKY) {
					surf->texturechain = skychain;
					skychain = surf;
				} else if (surf->flags & SURF_DRAWTURB) {
					surf->texturechain = waterchain;
					waterchain = surf;
				} else
					R_DrawSequentialPoly (surf);*/

			}
		}

	}

// recurse down the back side
	R_RecursiveWorldNode (node->children[!side]);
}

#if 0
qboolean R_CullSurface( msurface_t *surf, uint clipflags );

/*
================
R_CullNodeTopView

cull node by user rectangle (simple scissor)
================
*/
bool R_CullNodeTopView( mnode_t *node )
{
	vec2_t	delta, size;
	vec3_t	center, half;

	// build the node center and half-diagonal
	VectorAverage( node->minmaxs, node->minmaxs + 3, center );
	VectorSubtract( node->minmaxs + 3, center, half );

	// cull against the screen frustum or the appropriate area's frustum.
//	Vector2Subtract( center, world_orthocenter, delta );
//	Vector2Add( half, world_orthohalf, size );

	return ( fabs( delta[0] ) > size[0] ) || ( fabs( delta[1] ) > size[1] );
}

/*
================
R_DrawTopViewLeaf
================
*/
static void R_DrawTopViewLeaf( mleaf_t *pleaf, uint clipflags )
{
	msurface_t	**mark, *surf;
	int		i;

	for( i = 0, mark = pleaf->firstmarksurface; i < pleaf->nummarksurfaces; i++, mark++ )
	{
		surf = *mark;

		// don't process the same surface twice
		if( surf->visframe == r_framecount )
			continue;

		surf->visframe = r_framecount;

		if( R_CullSurface( surf, clipflags ))
			continue;

		if(!( surf->flags & SURF_DRAWSKY ))
		{
			surf->texturechain = surf->texinfo->texture->texturechain;
			surf->texinfo->texture->texturechain = surf;
		}
	}

	// deal with model fragments in this leaf
	if( pleaf->efrags )
		R_StoreEfrags( &pleaf->efrags );
}

/*
================
R_DrawWorldTopView
================
*/
void R_DrawWorldTopView( mnode_t *node, uint clipflags )
{
	const mplane_t	*clipplane;
	int		c, clipped;
	msurface_t	*surf;

	do
	{
		if( node->contents == CONTENTS_SOLID )
			return;	// hit a solid leaf

		if( node->visframe != r_visframecount )
			return;

		if( clipflags )
		{
			for( c = 0, clipplane = frustum; c < 6; c++, clipplane++ )
			{
				if(!( clipflags & ( 1<<c )))
					continue;

				clipped = BoxOnPlaneSide( node->minmaxs, node->minmaxs + 3, (mplane_t*)clipplane );
				if( clipped == 2 ) return;
				if( clipped == 1 ) clipflags &= ~(1<<c);
			}
		}

		// cull against the screen frustum or the appropriate area's frustum.
		if( R_CullNodeTopView( node ))
			return;

		// if a leaf node, draw stuff
		if( node->contents < 0 )
		{
			R_DrawTopViewLeaf( (mleaf_t *)node, clipflags );
			return;
		}

		// draw stuff
		for( c = node->numsurfaces, surf = cl.worldmodel->surfaces + node->firstsurface; c; c--, surf++ )
		{
			// don't process the same surface twice
			if( surf->visframe == r_framecount )
				continue;

			surf->visframe = r_framecount;

			if( R_CullSurface( surf, clipflags ))
				continue;

			if(!( surf->flags & SURF_DRAWSKY ))
			{
				surf->texturechain = surf->texinfo->texture->texturechain;
				surf->texinfo->texture->texturechain = surf;
			}
		}

		// recurse down both children, we don't care the order...
		R_DrawWorldTopView( node->children[0], clipflags );
		node = node->children[1];

	} while( node );
}
#endif

extern char	skybox_name[32];

/*
=============
R_DrawWorld
=============
*/
void R_DrawWorld (void)
{
	entity_t	ent;

	memset (&ent, 0, sizeof(ent));
	ent.model = cl.worldmodel;

	VectorCopy (r_refdef.vieworg, modelorg);

	currententity = &ent;
	currenttexture = -1;

	memset (lightmap_polys, 0, sizeof(lightmap_polys));

	R_ClearSkyBox ();
	R_RecursiveWorldNode (cl.worldmodel->nodes);
	DrawTextureChains ();
	R_BlendLightmaps ();
    if (skybox_name[0])
        R_DrawSkyBox ();
}


/*
===============
R_MarkLeaves
===============
*/
void R_MarkLeaves (void)
{
	byte	*vis;
	mnode_t	*node;
	int		i;
	byte	solid[4096];

	if (r_oldviewleaf == r_viewleaf && !r_novis.value)
		return;
	
	if (mirror)
		return;

	r_visframecount++;
	r_oldviewleaf = r_viewleaf;

	if (r_novis.value)
	{
		vis = solid;
		memset (solid, 0xff, (cl.worldmodel->numleafs+7)>>3);
	}
	else
		vis = Mod_LeafPVS (r_viewleaf, cl.worldmodel);
		
	for (i=0 ; i<cl.worldmodel->numleafs ; i++)
	{
		if (vis[i>>3] & (1<<(i&7)))
		{
			node = (mnode_t *)&cl.worldmodel->leafs[i+1];
			do
			{
				if (node->visframe == r_visframecount)
					break;
				node->visframe = r_visframecount;
				node = node->parent;
			} while (node);
		}
	}
}


/*
=============================================================================

  LIGHTMAP ALLOCATION

=============================================================================
*/

// returns a texture number and the position inside it
/*
static int AllocBlock (int w, int h, int *x, int *y)
{
	int		i, j;
	int		best, best2;
	int		texnum;

	for (texnum=0 ; texnum<MAX_LIGHTMAPS ; texnum++)
	{
		best = BLOCK_HEIGHT;

		for (i=0 ; i<BLOCK_WIDTH-w ; i++)
		{
			best2 = 0;

			for (j=0 ; j<w ; j++)
			{
				if (allocated[texnum][i+j] >= best)
					break;
				if (allocated[texnum][i+j] > best2)
					best2 = allocated[texnum][i+j];
			}
			if (j == w)
			{	// this is a valid spot
				*x = i;
				*y = best = best2;
			}
		}

		if (best + h > BLOCK_HEIGHT)
			continue;

		for (i=0 ; i<w ; i++)
			allocated[texnum][*x + i] = best + h;

		return texnum;
	}

	Sys_Error ("AllocBlock: full");
	return 0;
}
*/
/*
========================
AllocBlock -- returns a texture number and the position inside it
========================
*/
int AllocBlock (int w, int h, int *x, int *y)
{
	int		i, j;
	int		best, best2;
	int		texnum;

	// ericw -- rather than searching starting at lightmap 0 every time,
	// start at the last lightmap we allocated a surface in.
	// This makes AllocBlock much faster on large levels (can shave off 3+ seconds
	// of load time on a level with 180 lightmaps), at a cost of not quite packing
	// lightmaps as tightly vs. not doing this (uses ~5% more lightmaps)
	for (texnum=last_lightmap_allocated ; texnum<MAX_LIGHTMAPS ; texnum++, last_lightmap_allocated++)
	{
		best = BLOCK_HEIGHT;

		for (i=0 ; i<BLOCK_WIDTH-w ; i++)
		{
			best2 = 0;

			for (j=0 ; j<w ; j++)
			{
				if (allocated[texnum][i+j] >= best)
					break;
				if (allocated[texnum][i+j] > best2)
					best2 = allocated[texnum][i+j];
			}
			if (j == w)
			{	// this is a valid spot
				*x = i;
				*y = best = best2;
			}
		}

		if (best + h > BLOCK_HEIGHT)
			continue;

		for (i=0 ; i<w ; i++)
			allocated[texnum][*x + i] = best + h;

		return texnum;
	}

	Sys_Error ("AllocBlock: full");
	return 0; //johnfitz -- shut up compiler
}
mvertex_t	*r_pcurrentvertbase;
model_t		*currentmodel;

int	nColinElim;

/*
================
BuildSurfaceDisplayList
================
*/
static void BuildSurfaceDisplayList (msurface_t *fa)
{
	int			i, lindex, lnumverts;
	medge_t		*pedges, *r_pedge;
	int			vertpage;
	float		*vec;
	float		s, t;
	glpoly_t	*poly;

// reconstruct the polygon
	pedges = currentmodel->edges;
	lnumverts = fa->numedges;
	vertpage = 0;

	//
	// draw texture
	//
	poly = static_cast<glpoly_t*>(Hunk_Alloc (sizeof(glpoly_t) + (lnumverts * 2 - 1) * sizeof(glvert_t)));
	poly->next = fa->polys;
	poly->flags = fa->flags;
	fa->polys = poly;
	poly->numverts = lnumverts;

	for (i=0 ; i<lnumverts ; i++)
	{
		lindex = currentmodel->surfedges[fa->firstedge + i];

		if (lindex > 0)
		{
			r_pedge = &pedges[lindex];
			vec = r_pcurrentvertbase[r_pedge->v[0]].position;
		}
		else
		{
			r_pedge = &pedges[-lindex];
			vec = r_pcurrentvertbase[r_pedge->v[1]].position;
		}
		s = DotProduct (vec, fa->texinfo->vecs[0]) + fa->texinfo->vecs[0][3];
		s /= fa->texinfo->texture->width;

		t = DotProduct (vec, fa->texinfo->vecs[1]) + fa->texinfo->vecs[1][3];
		t /= fa->texinfo->texture->height;

		VectorCopy(vec, poly->verts[i].xyz);
		poly->verts[i].st[0] = s;
		poly->verts[i].st[1] = t;

		//
		// lightmap texture coordinates
		//
		s = DotProduct (vec, fa->texinfo->vecs[0]) + fa->texinfo->vecs[0][3];
		s -= fa->texturemins[0];
		s += fa->light_s*16;
		s += 8;
		s /= BLOCK_WIDTH*16; //fa->texinfo->texture->width;

		t = DotProduct (vec, fa->texinfo->vecs[1]) + fa->texinfo->vecs[1][3];
		t -= fa->texturemins[1];
		t += fa->light_t*16;
		t += 8;
		t /= BLOCK_HEIGHT*16; //fa->texinfo->texture->height;

		VectorCopy(vec, poly->verts[i + lnumverts].xyz);
		poly->verts[i + lnumverts].st[0] = s;
		poly->verts[i + lnumverts].st[1] = t;
	}

	//
	// remove co-linear points - Ed
	//
	
	// Colinear point removal-start
	
	int lm_vert_offset = lnumverts;
	
	if (!gl_keeptjunctions.value && !(fa->flags & SURF_UNDERWATER) )
	{
		int numRemoved = 0;
		int j;

		for (i = 0 ; i < lnumverts ; ++i)
		{
			vec3_t v1, v2;
			const glvert_t *prev, *this_, *next;

			prev = &poly->verts[(i + lnumverts - 1) % lnumverts];
			this_ = &poly->verts[i];
			next = &poly->verts[(i + 1) % lnumverts];

			VectorSubtract( this_->xyz, prev->xyz, v1 );
			VectorNormalize( v1 );
			VectorSubtract( next->xyz, prev->xyz, v2 );
			VectorNormalize( v2 );

			// skip co-linear points
			#define COLINEAR_EPSILON 0.001
			if ((fabsf( v1[0] - v2[0] ) <= COLINEAR_EPSILON) &&
				(fabsf( v1[1] - v2[1] ) <= COLINEAR_EPSILON) && 
				(fabsf( v1[2] - v2[2] ) <= COLINEAR_EPSILON))
			{
				for (j = i + 1; j < lnumverts; ++j)
				{
					poly->verts[j - 1] = poly->verts[j];
					poly->verts[lm_vert_offset + j - 1] = poly->verts[lm_vert_offset+j];
				} 
				
				--lnumverts;
				++nColinElim;
				numRemoved++;
				// retry next vertex next time, which is now current vertex
				--i;
			}
		}
		
		if (numRemoved > 0) {
			for (j = lm_vert_offset; j < lm_vert_offset + lnumverts; j++) {
				poly->verts[j - numRemoved] = poly->verts[j];
			}
		}
				
	}
	
	// Colinear point removal-end
	poly->numverts = lnumverts;

}

/*
========================
GL_CreateSurfaceLightmap
========================
*/
static void GL_CreateSurfaceLightmap (msurface_t *surf)
{
	int		smax, tmax;
	byte	*base;

	if( !cl.worldmodel->lightdata ) return;
	if( surf->flags & SURF_DRAWTILED )
		return;

	smax = (surf->extents[0]>>4)+1;
	tmax = (surf->extents[1]>>4)+1;

	surf->lightmaptexturenum = AllocBlock (smax, tmax, &surf->light_s, &surf->light_t);
	base = lightmaps + surf->lightmaptexturenum*LIGHTMAP_BYTES*BLOCK_WIDTH*BLOCK_HEIGHT;
	base += (surf->light_t * BLOCK_WIDTH + surf->light_s) * LIGHTMAP_BYTES;
	R_BuildLightMap (surf, base, BLOCK_WIDTH*LIGHTMAP_BYTES);
}


/*
==================
GL_BuildLightmaps

Builds the lightmap texture
with all the surfaces from all brush models
==================
*/
void GL_BuildLightmaps (void)
{
	int		i, j;
	model_t	*m;

	memset (allocated, 0, sizeof(allocated));
	
	r_framecount = 1;		// no dlightcache
	last_lightmap_allocated = 0;
	if (!lightmap_textures)
	{
		lightmap_textures = 0;
	}

	for (j=1 ; j<MAX_MODELS ; j++)
	{
		m = cl.model_precache[j];
		if (!m)
			break;
		if (m->name[0] == '*')
			continue;
		r_pcurrentvertbase = m->vertexes;
		currentmodel = m;
		for (i=0 ; i<m->numsurfaces ; i++)
		{
//Crow_bar decals
			// clearing all decal chains
			m->surfaces[i].pdecals = NULL;
			m->surfaces[i].visframe = 0;

			GL_CreateSurfaceLightmap (m->surfaces + i);
			if ( m->surfaces[i].flags & SURF_DRAWTURB )
				continue;
			if ( m->surfaces[i].flags & SURF_DRAWSKY )
				continue;
			BuildSurfaceDisplayList (m->surfaces + i);
		}
	}

	//
	// upload all lightmaps that were filled
	//
	char lm_name[16]; 
	for (i=0 ; i<MAX_LIGHTMAPS ; i++)
	{
		if (!allocated[i][0])
			break;		// no more used
		lightmap_modified[i] = qfalse;
		lightmap_rectchange[i].l = BLOCK_WIDTH;
		lightmap_rectchange[i].t = BLOCK_HEIGHT;
		lightmap_rectchange[i].w = 0;
		lightmap_rectchange[i].h = 0;
		
		sprintf(lm_name,"lightmap%d",i);
		lightmap_index[i] = GL_LoadTextureLM (lm_name, BLOCK_WIDTH, BLOCK_HEIGHT, lightmaps+(i*BLOCK_WIDTH*BLOCK_HEIGHT*LIGHTMAP_BYTES), LIGHTMAP_BYTES, GU_LINEAR, qtrue);
		
	}
}

