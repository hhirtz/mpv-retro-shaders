// Port of the CRT Guest Advanced shader (NTSC).
// The original shader is distributed under GPL-2.0.
//
// Copyright (c) 2023, The mpv-retro-shaders Contributors
// Copyright (c) 2019-2022 guest(r) and Dr. Venom
// Copyright (c) 2018-2023 guest(r) - guest.r@gmail.com
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, see <https://www.gnu.org/licenses/>


// Original shader is located at:
// https://github.com/libretro/slang-shaders/blob/5b4c9b2a75aba5f57fcb5b68662a06f0ed7c929f/crt/crt-guest-advanced-ntsc.slangp


// TODO check filter_linear values and how to enforce them in MPV
// TODO check mipmap_input values and how to enforce them in MPV


//!HOOK MAIN
//!SAVE AFTERGLOW0
//!BIND MAIN
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- AfterglowPass

// Parameters
#define PERSISTENCE_RED   0.32  // between 0.0 and 0.5
#define PERSISTENCE_GREEN 0.32  // between 0.0 and 0.5
#define PERSISTENCE_BLUE  0.32  // between 0.0 and 0.5

// libretro <-> mpv compatibility layer
#define TEX0 MAIN_pos
#define COMPAT_TEXTURE(c,d) texture(c,d)
#define OriginalHistory0 MAIN_raw
struct params_ {
	vec4 OriginalSize;
} params = params_(
	vec4(MAIN_size, MAIN_pt)
);
#define PR PERSISTENCE_RED
#define PG PERSISTENCE_GREEN
#define PB PERSISTENCE_BLUE

#ifndef linearize
// Implementation from mpv's gpu-next
vec4 linearize(vec4 color) {
       const float _const_0_1 = 0.05958483740687370300;
       const float _const_1_1 = 0.87031054496765136718;
       color.rgb = max(color.rgb, 0.0);
       color.rgb = _const_1_1 * pow(color.rgb + vec3(_const_0_1), vec3(2.4));
       return color;
}
#endif

vec4 hook() {
	vec4 FragColor = vec4(0.0);

	vec2 dx = vec2(params.OriginalSize.z, 0.0);
	vec2 dy = vec2(0.0, params.OriginalSize.w);

	vec3 color0  = linearize(COMPAT_TEXTURE(OriginalHistory0, TEX0.xy)).rgb;
	vec3 color1  = linearize(COMPAT_TEXTURE(OriginalHistory0, TEX0.xy - dx)).rgb;
	vec3 color2  = linearize(COMPAT_TEXTURE(OriginalHistory0, TEX0.xy + dx)).rgb;
	vec3 color3  = linearize(COMPAT_TEXTURE(OriginalHistory0, TEX0.xy - dy)).rgb;
	vec3 color4  = linearize(COMPAT_TEXTURE(OriginalHistory0, TEX0.xy + dy)).rgb;

	vec3 color = (2.5 * color0 + color1 + color2 + color3 + color4)/6.5;

	// TODO PassFeedpack is meant to be the output of this pass at the previous frame. Change this when mpv supports this feature
	//vec3 accumulate = COMPAT_TEXTURE(AfterglowPassFeedback, TEX0.xy).rgb;
	vec3 accumulate = color;

	float w = 1.0;
	if ((color0.r + color0.g + color0.b < 5.0/255.0)) { w = 0.0; }

	vec3 result = mix( max(mix(color, accumulate, 0.49 + vec3(PR, PG, PB))- 1.25/255.0, 0.0), color, w);

	FragColor = vec4(result, w);

	return FragColor;
}


//!HOOK MAIN
//!SAVE AFTERGLOW
//!BIND MAIN
//!BIND AFTERGLOW0
//!BIND SamplerLUT1
//!BIND SamplerLUT2
//!BIND SamplerLUT3
//!BIND SamplerLUT4
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- PrePass0

// Parameters
#define AFTERGLOW_STRENGTH    0.2    // between 0.0 and 0.6
#define AFTERGLOW_SATURATION  0.5    // between 0.0 and 1.0
#define CRT_STYLE             0      // either 0 (sRGB), 1 (modern), 2 (DCI), 3 (adobe) or 4 (rec.2020)
#define CRT_PROFILE           0      // either 0 (EBU), 1 (P22), 2 (SMPTE-C), 3 (Philips) or 4 (trin)
#define TNTC                  1      // either 0 (trin.1), 1 (trin.2), 2 (nec mult.) or 3 (ntsc)
#define LUT_SIZE              32     // either 16, 32, 48 or 64
#define LUT_LOW               5.0    // between 0.0 and 50.0
#define LUT_BRIGHTNESS        1.0    // between 0.0 and 1.0
#define COLOR_TEMPERATURE     0.0    // between -100.0 and 100.0
#define SATURATION_ADJUSTMENT 1.0    // between 0.0 and 2.0
#define BRIGHTNESS_ADJUSTMENT 1.0    // between 0.0 and 2.0
#define CONTRAST_ADJUSTMENT   0.0    // between -2.0 and 2.0
#define SEGA_FIX              false  // boolean
#define BLACK_LEVEL           0.0    // between -100.0 and 25.0
#define VIGNETTE_STRENGTH     0.0    // between 0.0 and 2.0
#define VIGNETTE_SIZE         1.0    // between 0.5 and 3.0

// libretro <-> mpv compatibility layer
#define COMPAT_TEXTURE(c,d) texture(c,d)
#define vTexCoord MAIN_pos
#define StockPass MAIN_raw
#define AfterglowPass AFTERGLOW0_raw
struct params_ {
	vec4 OriginalSize;
	float vigstr;
	float vigdef;
	float sega_fix;
	float pre_bb;
	float contr;
} params = params_(
	vec4(MAIN_size, MAIN_pt),
	VIGNETTE_STRENGTH,
	VIGNETTE_SIZE,
	float(SEGA_FIX),
	BRIGHTNESS_ADJUSTMENT,
	CONTRAST_ADJUSTMENT
);
#define AS AFTERGLOW_STRENGTH
#define sat AFTERGLOW_SATURATION
#define CS float(CRT_STYLE)
#define CP float(CRT_PROFILE)
#define LS float(LUT_SIZE)
#define LS float(LUT_SIZE)
#define LUTLOW LUT_LOW
#define LUTBR LUT_BRIGHTNESS
#define WP COLOR_TEMPERATURE
#define wp_saturation SATURATION_ADJUSTMENT
#define BP BLACK_LEVEL

#ifndef linearize
// Implementation from mpv's gpu-next
vec4 linearize(vec4 color) {
       const float _const_0_1 = 0.05958483740687370300;
       const float _const_1_1 = 0.87031054496765136718;
       color.rgb = max(color.rgb, 0.0);
       color.rgb = _const_1_1 * pow(color.rgb + vec3(_const_0_1), vec3(2.4));
       return color;
}
#endif

// Color profile matrices

const mat3 Profile0 =
mat3(
 0.412391,  0.212639,  0.019331,
 0.357584,  0.715169,  0.119195,
 0.180481,  0.072192,  0.950532
);

const mat3 Profile1 =
mat3(
 0.430554,  0.222004,  0.020182,
 0.341550,  0.706655,  0.129553,
 0.178352,  0.071341,  0.939322
);

const mat3 Profile2 =
mat3(
 0.396686,  0.210299,  0.006131,
 0.372504,  0.713766,  0.115356,
 0.181266,  0.075936,  0.967571
);

const mat3 Profile3 =
mat3(
 0.393521,  0.212376,  0.018739,
 0.365258,  0.701060,  0.111934,
 0.191677,  0.086564,  0.958385
);

const mat3 Profile4 =
mat3(
 0.392258,  0.209410,  0.016061,
 0.351135,  0.725680,  0.093636,
 0.166603,  0.064910,  0.850324
);

const mat3 Profile5 =
mat3(
 0.377923,  0.195679,  0.010514,
 0.317366,  0.722319,  0.097826,
 0.207738,  0.082002,  1.076960
);

const mat3 ToSRGB =
mat3(
 3.240970, -0.969244,  0.055630,
-1.537383,  1.875968, -0.203977,
-0.498611,  0.041555,  1.056972
);

const mat3 ToModern =
mat3(
 2.791723,	-0.894766,	0.041678,
-1.173165,	 1.815586, -0.130886,
-0.440973,	 0.032000,	1.002034
);

const mat3 ToDCI =
mat3(
 2.493497,	-0.829489,	0.035846,
-0.931384,	 1.762664, -0.076172,
-0.402711,	 0.023625,	0.956885
);

const mat3 ToAdobe =
mat3(
 2.041588, -0.969244,  0.013444,
-0.565007,  1.875968, -0.11836,
-0.344731,  0.041555,  1.015175
);

const mat3 ToREC =
mat3(
 1.716651, -0.666684,  0.017640,
-0.355671,  1.616481, -0.042771,
-0.253366,  0.015769,  0.942103
);

// Color temperature matrices

const mat3 D65_to_D55 = mat3 (
           0.4850339153,  0.2500956126,  0.0227359648,
           0.3488957224,  0.6977914447,  0.1162985741,
           0.1302823568,  0.0521129427,  0.6861537456);


const mat3 D65_to_D93 = mat3 (
           0.3412754080,  0.1759701322,  0.0159972847,
           0.3646170520,  0.7292341040,  0.1215390173,
           0.2369894093,  0.0947957637,  1.2481442225);


vec3 fix_lut(vec3 lutcolor, vec3 ref)
{
	float r = length(ref);
	float l = length(lutcolor);
	float m = max(max(ref.r,ref.g),ref.b);
	ref = normalize(lutcolor + 0.0000001) * mix(r, l, pow(m,1.25));
	return mix(lutcolor, ref, LUTBR);
}


float vignette(vec2 pos) {
	vec2 b = vec2(params.vigdef, params.vigdef) *  vec2(1.0, params.OriginalSize.x/params.OriginalSize.y) * 0.125;
	pos = clamp(pos, 0.0, 1.0);
	pos = abs(2.0*(pos - 0.5));
	vec2 res = mix(0.0.xx, 1.0.xx, smoothstep(1.0.xx, 1.0.xx-b, sqrt(pos)));
	res = pow(res, 0.70.xx);
	return max(mix(1.0, sqrt(res.x*res.y), params.vigstr), 0.0);
}


vec3 plant (vec3 tar, float r)
{
	float t = max(max(tar.r,tar.g),tar.b) + 0.00001;
	return tar * r / t;
}

float contrast(float x)
{
	return max(mix(x, smoothstep(0.0, 1.0, x), params.contr),0.0);
}

vec4 hook() {
	vec4 FragColor = vec4(0.0);

   vec4 imgColor = linearize(COMPAT_TEXTURE(StockPass, vTexCoord.xy));
   vec4 aftglow = COMPAT_TEXTURE(AfterglowPass, vTexCoord.xy);

   float w = 1.0-aftglow.w;

   float l = length(aftglow.rgb);
   aftglow.rgb = AS*w*normalize(pow(aftglow.rgb + 0.01, vec3(sat)))*l;
   float bp = w * BP/255.0;

   if (params.sega_fix > 0.5) imgColor.rgb = imgColor.rgb * (255.0 / 239.0);

   imgColor.rgb = min(imgColor.rgb, 1.0);

   vec3 color = imgColor.rgb;

   if (int(TNTC) == 0)
   {
      color.rgb = imgColor.rgb;
   }
   else
   {
	  float lutlow = LUTLOW/255.0; float invLS = 1.0/LS;
	  vec3 lut_ref = imgColor.rgb + lutlow*(1.0 - pow(imgColor.rgb, 0.333.xxx));
	  float lutb = lut_ref.b * (1.0-0.5*invLS);
	  lut_ref.rg    = lut_ref.rg * (1.0-invLS) + 0.5*invLS;
	  float tile1 = ceil (lutb * (LS-1.0));
	  float tile0 = max(tile1 - 1.0, 0.0);
	  float f = fract(lutb * (LS-1.0)); if (f == 0.0) f = 1.0;
	  vec2 coord0 = vec2(tile0 + lut_ref.r, lut_ref.g)*vec2(invLS, 1.0);
	  vec2 coord1 = vec2(tile1 + lut_ref.r, lut_ref.g)*vec2(invLS, 1.0);
	  vec4 color1, color2, res;

      if (int(TNTC) == 1)
      {
         color1 = COMPAT_TEXTURE(SamplerLUT1, coord0);
         color2 = COMPAT_TEXTURE(SamplerLUT1, coord1);
         res = mix(color1, color2, f);
      }
      else if (int(TNTC) == 2)
      {
         color1 = COMPAT_TEXTURE(SamplerLUT2, coord0);
         color2 = COMPAT_TEXTURE(SamplerLUT2, coord1);
         res = mix(color1, color2, f);
      }
      else if (int(TNTC) == 3)
      {
         color1 = COMPAT_TEXTURE(SamplerLUT3, coord0);
         color2 = COMPAT_TEXTURE(SamplerLUT3, coord1);
         res = mix(color1, color2, f);
      }
      else if (int(TNTC) == 4)
      {
         color1 = COMPAT_TEXTURE(SamplerLUT4, coord0);
         color2 = COMPAT_TEXTURE(SamplerLUT4, coord1);
         res = mix(color1, color2, f);
      }

      res.rgb = fix_lut (res.rgb, imgColor.rgb);

      color = mix(imgColor.rgb, res.rgb, min(TNTC,1.0));
   }

	vec3 c = clamp(color, 0.0, 1.0);

	float p;
	mat3 m_out;

	if (CS == 0.0) { p = 2.2; m_out =  ToSRGB;   } else
	if (CS == 1.0) { p = 2.2; m_out =  ToModern; } else
	if (CS == 2.0) { p = 2.6; m_out =  ToDCI;    } else
	if (CS == 3.0) { p = 2.2; m_out =  ToAdobe;  } else
	if (CS == 4.0) { p = 2.4; m_out =  ToREC;    }

	color = pow(c, vec3(p));

	mat3 m_in = Profile0;

	if (CP == 0.0) { m_in = Profile0; } else
	if (CP == 1.0) { m_in = Profile1; } else
	if (CP == 2.0) { m_in = Profile2; } else
	if (CP == 3.0) { m_in = Profile3; } else
	if (CP == 4.0) { m_in = Profile4; } else
	if (CP == 5.0) { m_in = Profile5; }

	color = m_in*color;
	color = m_out*color;

	color = clamp(color, 0.0, 1.0);

	color = pow(color, vec3(1.0/p));

	if (CP == -1.0) color = c;

	vec3 scolor1 = plant(pow(color, vec3(wp_saturation)), max(max(color.r,color.g),color.b));
	float luma = dot(color, vec3(0.299, 0.587, 0.114));
	vec3 scolor2 = mix(vec3(luma), color, wp_saturation);
	color = (wp_saturation > 1.0) ? scolor1 : scolor2;

	color = plant(color, contrast(max(max(color.r,color.g),color.b)));

	p = 2.2;
	color = clamp(color, 0.0, 1.0);
	color = pow(color, vec3(p));

	vec3 warmer = D65_to_D55*color;
	warmer = ToSRGB*warmer;

	vec3 cooler = D65_to_D93*color;
	cooler = ToSRGB*cooler;

	float m = abs(WP)/100.0;

	vec3 comp = (WP < 0.0) ? cooler : warmer;

	color = mix(color, comp, m);
	color = pow(max(color, 0.0), vec3(1.0/p));

	if (BP > -0.5) color = color + aftglow.rgb + bp; else
	{
		color = max(color + BP/255.0, 0.0) / (1.0 + BP/255.0*step(- BP/255.0, max(max(color.r,color.g),color.b))) + aftglow.rgb;
	}

	color = min(color * params.pre_bb, 1.0);

	FragColor = vec4(color, vignette(vTexCoord.xy));

	return FragColor;
}


//!HOOK MAIN
//!SAVE NTSC1
//!BIND MAIN
//!BIND AFTERGLOW
//!WIDTH 4.0 MAIN.w *
//!HEIGHT MAIN.h
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- NPass1

// Parameters
#define CUST_ARTIFACTING 1.0  // between 0.0 and 5.0
#define CUST_FRINGING    1.0  // between 0.0 and 5.0
#define NTSC_BRIGHTNESS  1.0  // between 0.0 and 1.5
#define NTSC_FIELDS      -1   // either -1 (auto), 0 (no) or 1 (yes)
#define NTSC_PHASE       1    // either 1 (auto), 2 phases, 3 phases, or 4 (mixed)
#define NTSC_SATURATION  1.0  // between 0.0 and 2.0
#define NTSC_SCALE       1.0  // between 0.2 and 2.5

// libretro <-> mpv compatibility layer
#define COMPAT_TEXTURE(c,d) texture(c,d)
#define vTexCoord MAIN_pos
#define Source AFTERGLOW_raw
struct params_ {
	vec4 OriginalSize;
	uint FrameCount;
	float ntsc_scale;
	float ntsc_phase;
} global = params_(
	vec4(MAIN_size, MAIN_pt),
	uint(frame),
	NTSC_SCALE,
	float(NTSC_PHASE)
);
#define FRINGING CUST_FRINGING
#define ARTIFACTING CUST_ARTIFACTING
#define BRIGHTNESS NTSC_BRIGHTNESS
#define SATURATION NTSC_SATURATION

#define PI 3.14159265

#define mix_mat  mat3(BRIGHTNESS, FRINGING, FRINGING, ARTIFACTING, 2.0 * SATURATION, 0.0, ARTIFACTING, 0.0, 2.0 * SATURATION)

const mat3 yiq2rgb_mat = mat3(
   1.0, 0.956, 0.6210,
   1.0, -0.2720, -0.6474,
   1.0, -1.1060, 1.7046);

vec3 yiq2rgb(vec3 yiq)
{
   return yiq * yiq2rgb_mat;
}

const mat3 yiq_mat = mat3(
      0.2989, 0.5870, 0.1140,
      0.5959, -0.2744, -0.3216,
      0.2115, -0.5229, 0.3114
);

vec3 rgb2yiq(vec3 col)
{
   return col * yiq_mat;
}

vec4 hook() {
	// Vertex shader

	float ntsc_scale = min(NTSC_SCALE, 1.0);
	vec2 pix_no = AFTERGLOW_pos * AFTERGLOW_size * (ntsc_scale * target_size / AFTERGLOW_size);
	float phase = (NTSC_PHASE <= 1)
		? ((MAIN_size.x > 300.0) ? 2 : 3)
		: ((NTSC_PHASE >= 3) ? 3 : 2);
	float CHROMA_MOD_FREQ = (phase <= 2)
		? 4.0 * PI / 15.0
		: PI / 3.0;
	float MERGE = float((NTSC_FIELDS == -1 && phase == 3) || (NTSC_FIELDS == 1));

	// Fragment shader

	vec4 FragColor = vec4(0.0);

   float res = global.ntsc_scale;
   vec3 col = texture(Source, vTexCoord).rgb;

   vec3 yiq = rgb2yiq(col);
   float lum = yiq.x;

if (global.ntsc_phase == 4.0)
{
   vec2 dx = vec2(global.OriginalSize.z, 0.0);
   vec3 c1 = texture(Source, vTexCoord - dx).rgb;
   vec3 c2 = texture(Source, vTexCoord + dx).rgb;
   c1 = rgb2yiq(c1);
   c2 = rgb2yiq(c2);
   yiq.x = mix(min(0.5*(yiq.x + max(c1.x,c2.x)), max(yiq.x , min(c1.x,c2.x))), yiq.x, 5.0*min(abs(c1.x-c2.x),1.0));
}
   vec3 yiq2 = yiq;
   vec3 yiqs = yiq;
   vec3 yiqz = yiq;

   float mod1 = 2.0;
   float mod2 = 3.0;

if (MERGE > 0.5)
{
   float chroma_phase2 = (phase < 2.5) ? PI * (mod(pix_no.y, mod1) + mod(global.FrameCount+1, 2.)) : 0.6667 * PI * (mod(pix_no.y, mod2) + mod(global.FrameCount+1, 2.));
   float mod_phase2 = chroma_phase2 + pix_no.x * CHROMA_MOD_FREQ;
   float i_mod2 = cos(mod_phase2);
   float q_mod2 = sin(mod_phase2);
   yiq2.yz *= vec2(i_mod2, q_mod2); // Modulate.
   yiq2 *= mix_mat; // Cross-talk.
   yiq2.yz *= vec2(i_mod2, q_mod2); // Demodulate.

   if (res > 1.025)
   {
      mod_phase2 = chroma_phase2 + pix_no.x * CHROMA_MOD_FREQ * res;
      i_mod2 = cos(mod_phase2);
      q_mod2 = sin(mod_phase2);
      yiqs.yz *= vec2(i_mod2, q_mod2); // Modulate.
      yiq2.x = dot(yiqs, mix_mat[0]);  // Cross-talk.
   }
}

   float chroma_phase = (phase < 2.5) ? PI * (mod(pix_no.y, mod1) + mod(global.FrameCount, 2.)) : 0.6667 * PI * (mod(pix_no.y, mod2) + mod(global.FrameCount, 2.));
   float mod_phase = chroma_phase + pix_no.x * CHROMA_MOD_FREQ;

   float i_mod = cos(mod_phase);
   float q_mod = sin(mod_phase);

   yiq.yz *= vec2(i_mod, q_mod); // Modulate.
   yiq *= mix_mat; // Cross-talk.
   yiq.yz *= vec2(i_mod, q_mod); // Demodulate.

    if (res > 1.025)
   {
      mod_phase = chroma_phase + pix_no.x * CHROMA_MOD_FREQ * res;
      i_mod = cos(mod_phase);
      q_mod = sin(mod_phase);
      yiqz.yz *= vec2(i_mod, q_mod); // Modulate.
      yiq.x = dot(yiqz, mix_mat[0]); // Cross-talk.
   }

if (global.ntsc_phase == 4.0)
{
	yiq.x = lum; yiq2.x = lum;
}

   yiq = (MERGE < 0.5) ? yiq : 0.5*(yiq+yiq2);

   FragColor = vec4(yiq, lum);

	return FragColor;
}


//!HOOK MAIN
//!SAVE NTSC2
//!BIND MAIN
//!BIND AFTERGLOW
//!BIND NTSC1
//!WIDTH 0.5 NTSC1.w *
//!HEIGHT NTSC1.h
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- NPass2

// Parameters
#define NTSC_CSCALE 1.0  // between 0.2 and 2.5
#define NTSC_PHASE  1    // either 1 (auto), 2 phases, 3 phases, or 4 (mixed)
#define NTSC_RING   0.5  // between 0.0 and 1.0
#define NTSC_SCALE  1.0  // between 0.2 and 2.5

// libretro <-> mpv compatibility layer
#define COMPAT_TEXTURE(c,d) texture(c,d)
#define PrePass0 AFTERGLOW_raw
#define Source NTSC1_raw
struct params_ {
	vec4 OriginalSize;
	vec4 SourceSize;
	float ntsc_scale;
	float ntsc_phase;
	float ntsc_ring;
	float ntsc_cscale;
} global = params_(
	vec4(MAIN_size, MAIN_pt),
	vec4(NTSC1_size, NTSC1_pt),
	NTSC_SCALE,
	float(NTSC_PHASE),
	NTSC_RING,
	NTSC_CSCALE
);
vec2 vTexCoord = NTSC1_pos;

vec3 fetch_offset(float offset, float one_x)
{
   return vec3(texture(Source, vTexCoord + vec2((offset) * (one_x), 0.0)).x, texture(Source, vTexCoord + vec2((offset) * (one_x/global.ntsc_cscale), 0.0)).yz);
}

const mat3 yiq2rgb_mat = mat3(
   1.0, 0.956, 0.6210,
   1.0, -0.2720, -0.6474,
   1.0, -1.1060, 1.7046);

vec3 yiq2rgb(vec3 yiq)
{
   return yiq * yiq2rgb_mat;
}

const mat3 yiq_mat = mat3(
      0.2989, 0.5870, 0.1140,
      0.5959, -0.2744, -0.3216,
      0.2115, -0.5229, 0.3114
);

vec3 rgb2yiq(vec3 col)
{
   return col * yiq_mat;
}


const int TAPS_2_phase = 32;
const float luma_filter_2_phase[33] = float[33](
   -0.000174844,
   -0.000205844,
   -0.000149453,
   -0.000051693,
   0.000000000,
   -0.000066171,
   -0.000245058,
   -0.000432928,
   -0.000472644,
   -0.000252236,
   0.000198929,
   0.000687058,
   0.000944112,
   0.000803467,
   0.000363199,
   0.000013422,
   0.000253402,
   0.001339461,
   0.002932972,
   0.003983485,
   0.003026683,
   -0.001102056,
   -0.008373026,
   -0.016897700,
   -0.022914480,
   -0.021642347,
   -0.008863273,
   0.017271957,
   0.054921920,
   0.098342579,
   0.139044281,
   0.168055832,
   0.178571429);



const int TAPS_3_phase = 24;
const float luma_filter_3_phase[25] = float[25](
   -0.000012020,
   -0.000022146,
   -0.000013155,
   -0.000012020,
   -0.000049979,
   -0.000113940,
   -0.000122150,
   -0.000005612,
   0.000170516,
   0.000237199,
   0.000169640,
   0.000285688,
   0.000984574,
   0.002018683,
   0.002002275,
   -0.005909882,
   -0.012049081,
   -0.018222860,
   -0.022606931,
   0.002460860,
   0.035868225,
   0.084016453,
   0.135563500,
   0.175261268,
   0.220176552);

const float chroma_filter_3_phase[25] = float[25](
   -0.000118847,
   -0.000271306,
   -0.000502642,
   -0.000930833,
   -0.001451013,
   -0.002064744,
   -0.002700432,
   -0.003241276,
   -0.003524948,
   -0.003350284,
   -0.002491729,
   -0.000721149,
   0.002164659,
   0.006313635,
   0.011789103,
   0.018545660,
   0.026414396,
   0.035100710,
   0.044196567,
   0.053207202,
   0.061590275,
   0.068803602,
   0.074356193,
   0.077856564,
   0.079052396);


const float chroma_filter_4_phase[33] = float[33](
    0.0,
	0.0,
	0.0,
	0.0,
	0.0,
	0.0,
	0.0,
	0.0,
   -0.000118847,
   -0.000271306,
   -0.000502642,
   -0.000930833,
   -0.001451013,
   -0.002064744,
   -0.002700432,
   -0.003241276,
   -0.003524948,
   -0.003350284,
   -0.002491729,
   -0.000721149,
   0.002164659,
   0.006313635,
   0.011789103,
   0.018545660,
   0.026414396,
   0.035100710,
   0.044196567,
   0.053207202,
   0.061590275,
   0.068803602,
   0.074356193,
   0.077856564,
   0.079052396);

vec4 hook() {
	vec4 FragColor = vec4(0.0);

float chroma_filter_2_phase[33] = float[33](
   0.001384762,
   0.001678312,
   0.002021715,
   0.002420562,
   0.002880460,
   0.003406879,
   0.004004985,
   0.004679445,
   0.005434218,
   0.006272332,
   0.007195654,
   0.008204665,
   0.009298238,
   0.010473450,
   0.011725413,
   0.013047155,
   0.014429548,
   0.015861306,
   0.017329037,
   0.018817382,
   0.020309220,
   0.021785952,
   0.023227857,
   0.024614500,
   0.025925203,
   0.027139546,
   0.028237893,
   0.029201910,
   0.030015081,
   0.030663170,
   0.031134640,
   0.031420995,
   0.031517031);

   float res = global.ntsc_scale;
   float OriginalSize = global.OriginalSize.x;
   float one_x = global.SourceSize.z / res;
   vec3 signal = vec3(0.0);
   float phase = (global.ntsc_phase < 1.5) ? ((OriginalSize > 300.0) ? 2.0 : 3.0) : ((global.ntsc_phase > 2.5) ? 3.0 : 2.0);
   if (global.ntsc_phase == 4.0) { phase = 2.0; chroma_filter_2_phase = chroma_filter_4_phase; }


   if(phase < 2.5)
   {
      vec3 sums = fetch_offset(0.0 - 32.0, one_x) + fetch_offset(32.0 - 0.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[0], chroma_filter_2_phase[0], chroma_filter_2_phase[0]);
      sums = fetch_offset(1.0 - 32.0, one_x) + fetch_offset(32.0 - 1.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[1], chroma_filter_2_phase[1], chroma_filter_2_phase[1]);
      sums = fetch_offset(2.0 - 32.0, one_x) + fetch_offset(32.0 - 2.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[2], chroma_filter_2_phase[2], chroma_filter_2_phase[2]);
      sums = fetch_offset(3.0 - 32.0, one_x) + fetch_offset(32.0 - 3.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[3], chroma_filter_2_phase[3], chroma_filter_2_phase[3]);
      sums = fetch_offset(4.0 - 32.0, one_x) + fetch_offset(32.0 - 4.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[4], chroma_filter_2_phase[4], chroma_filter_2_phase[4]);
      sums = fetch_offset(5.0 - 32.0, one_x) + fetch_offset(32.0 - 5.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[5], chroma_filter_2_phase[5], chroma_filter_2_phase[5]);
      sums = fetch_offset(6.0 - 32.0, one_x) + fetch_offset(32.0 - 6.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[6], chroma_filter_2_phase[6], chroma_filter_2_phase[6]);
      sums = fetch_offset(7.0 - 32.0, one_x) + fetch_offset(32.0 - 7.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[7], chroma_filter_2_phase[7], chroma_filter_2_phase[7]);
      sums = fetch_offset(8.0 - 32.0, one_x) + fetch_offset(32.0 - 8.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[8], chroma_filter_2_phase[8], chroma_filter_2_phase[8]);
      sums = fetch_offset(9.0 - 32.0, one_x) + fetch_offset(32.0 - 9.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[9], chroma_filter_2_phase[9], chroma_filter_2_phase[9]);
      sums = fetch_offset(10.0 - 32.0, one_x) + fetch_offset(32.0 - 10.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[10], chroma_filter_2_phase[10], chroma_filter_2_phase[10]);
      sums = fetch_offset(11.0 - 32.0, one_x) + fetch_offset(32.0 - 11.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[11], chroma_filter_2_phase[11], chroma_filter_2_phase[11]);
      sums = fetch_offset(12.0 - 32.0, one_x) + fetch_offset(32.0 - 12.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[12], chroma_filter_2_phase[12], chroma_filter_2_phase[12]);
      sums = fetch_offset(13.0 - 32.0, one_x) + fetch_offset(32.0 - 13.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[13], chroma_filter_2_phase[13], chroma_filter_2_phase[13]);
      sums = fetch_offset(14.0 - 32.0, one_x) + fetch_offset(32.0 - 14.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[14], chroma_filter_2_phase[14], chroma_filter_2_phase[14]);
      sums = fetch_offset(15.0 - 32.0, one_x) + fetch_offset(32.0 - 15.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[15], chroma_filter_2_phase[15], chroma_filter_2_phase[15]);
      sums = fetch_offset(16.0 - 32.0, one_x) + fetch_offset(32.0 - 16.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[16], chroma_filter_2_phase[16], chroma_filter_2_phase[16]);
      sums = fetch_offset(17.0 - 32.0, one_x) + fetch_offset(32.0 - 17.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[17], chroma_filter_2_phase[17], chroma_filter_2_phase[17]);
      sums = fetch_offset(18.0 - 32.0, one_x) + fetch_offset(32.0 - 18.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[18], chroma_filter_2_phase[18], chroma_filter_2_phase[18]);
      sums = fetch_offset(19.0 - 32.0, one_x) + fetch_offset(32.0 - 19.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[19], chroma_filter_2_phase[19], chroma_filter_2_phase[19]);
      sums = fetch_offset(20.0 - 32.0, one_x) + fetch_offset(32.0 - 20.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[20], chroma_filter_2_phase[20], chroma_filter_2_phase[20]);
      sums = fetch_offset(21.0 - 32.0, one_x) + fetch_offset(32.0 - 21.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[21], chroma_filter_2_phase[21], chroma_filter_2_phase[21]);
      sums = fetch_offset(22.0 - 32.0, one_x) + fetch_offset(32.0 - 22.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[22], chroma_filter_2_phase[22], chroma_filter_2_phase[22]);
      sums = fetch_offset(23.0 - 32.0, one_x) + fetch_offset(32.0 - 23.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[23], chroma_filter_2_phase[23], chroma_filter_2_phase[23]);
      sums = fetch_offset(24.0 - 32.0, one_x) + fetch_offset(32.0 - 24.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[24], chroma_filter_2_phase[24], chroma_filter_2_phase[24]);
      sums = fetch_offset(25.0 - 32.0, one_x) + fetch_offset(32.0 - 25.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[25], chroma_filter_2_phase[25], chroma_filter_2_phase[25]);
      sums = fetch_offset(26.0 - 32.0, one_x) + fetch_offset(32.0 - 26.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[26], chroma_filter_2_phase[26], chroma_filter_2_phase[26]);
      sums = fetch_offset(27.0 - 32.0, one_x) + fetch_offset(32.0 - 27.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[27], chroma_filter_2_phase[27], chroma_filter_2_phase[27]);
      sums = fetch_offset(28.0 - 32.0, one_x) + fetch_offset(32.0 - 28.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[28], chroma_filter_2_phase[28], chroma_filter_2_phase[28]);
      sums = fetch_offset(29.0 - 32.0, one_x) + fetch_offset(32.0 - 29.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[29], chroma_filter_2_phase[29], chroma_filter_2_phase[29]);
      sums = fetch_offset(30.0 - 32.0, one_x) + fetch_offset(32.0 - 30.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[30], chroma_filter_2_phase[30], chroma_filter_2_phase[30]);
      sums = fetch_offset(31.0 - 32.0, one_x) + fetch_offset(32.0 - 31.0, one_x);
      signal += sums * vec3(luma_filter_2_phase[31], chroma_filter_2_phase[31], chroma_filter_2_phase[31]);

      signal += texture(Source, vTexCoord).xyz *
         vec3(luma_filter_2_phase[TAPS_2_phase], chroma_filter_2_phase[TAPS_2_phase], chroma_filter_2_phase[TAPS_2_phase]);
   }
   else if(phase > 2.5)
   {
      for (int i = 0; i < TAPS_3_phase; i++)
      {
         float offset = float(i);

         vec3 sums = fetch_offset(offset - float(TAPS_3_phase), one_x) +
            fetch_offset(float(TAPS_3_phase) - offset, one_x);
         signal += sums * vec3(luma_filter_3_phase[i], chroma_filter_3_phase[i], chroma_filter_3_phase[i]);
      }
      signal += texture(Source, vTexCoord).xyz *
         vec3(luma_filter_3_phase[TAPS_3_phase], chroma_filter_3_phase[TAPS_3_phase], chroma_filter_3_phase[TAPS_3_phase]);
   }


   if (global.ntsc_ring > 0.05)
   {
      vec2 dx = vec2(global.OriginalSize.z / min(res, 1.0), 0.0);
	  float a = texture(Source, vTexCoord - 1.5*dx).a;
	  float b = texture(Source, vTexCoord - 0.5*dx).a;
	  float c = texture(Source, vTexCoord + 1.5*dx).a;
	  float d = texture(Source, vTexCoord + 0.5*dx).a;
      float e = texture(Source, vTexCoord         ).a;
      signal.x = mix(signal.x, clamp(signal.x, min(min(min(a,b),min(c,d)),e), max(max(max(a,b),max(c,d)),e)), global.ntsc_ring);
   }


   vec3 orig = rgb2yiq(texture(PrePass0, vTexCoord).rgb);

   signal.x = clamp(signal.x, -1.0, 1.0);
   vec3 rgb = signal;

   FragColor = vec4(rgb, orig.x);

	return FragColor;
}


//!HOOK MAIN
//!SAVE NTSC3
//!BIND MAIN
//!BIND AFTERGLOW
//!BIND NTSC2
//!WIDTH NTSC2.w
//!HEIGHT NTSC2.h
//!DESC CRT Guest Advanced NTSC -- NPass3

// Parameters
#define NTSC_SHARP 0.0   // between -10.0 and 10.0
#define NTSC_SHAPE 0.75  // between 0.5 and 1.0
#define BLEND_MODE 1     // either 0 or 1

// libretro <-> mpv compatibility layer
#define COMPAT_TEXTURE(c,d) texture(c,d)
#define Source NTSC2_raw
#define PrePass0 AFTERGLOW_raw
struct params_ {
	vec4 OriginalSize;
	vec4 SourceSize;
	float ntsc_sharp;
	float ntsc_shape;
	float blendMode;
} global = params_(
	vec4(MAIN_size, MAIN_pt),
	vec4(NTSC2_size, NTSC2_pt),
	NTSC_SHARP,
	NTSC_SHAPE,
	float(BLEND_MODE)
);
vec2 vTexCoord = NTSC2_pos;

const mat3 yiq2rgb_mat = mat3(
   1.0, 0.956, 0.6210,
   1.0, -0.2720, -0.6474,
   1.0, -1.1060, 1.7046);

vec3 yiq2rgb(vec3 yiq)
{
   return yiq * yiq2rgb_mat;
}

const mat3 yiq_mat = mat3(
      0.2989, 0.5870, 0.1140,
      0.5959, -0.2744, -0.3216,
      0.2115, -0.5229, 0.3114
);

vec3 rgb2yiq(vec3 col)
{
   return col * yiq_mat;
}

vec4 hook() {
	vec4 FragColor = vec4(0.0);

   vec2 offsetx = vec2(0.5 * global.OriginalSize.z, 0.0);
   vec2 dx = vec2(0.25 * global.SourceSize.z, 0.0);
   vec2 texcoord = vTexCoord + dx;

   vec3 l1  = texture(Source, texcoord +      offsetx).xyz;
   vec3 l2  = texture(Source, texcoord -      offsetx).xyz;
   vec3 l3  = texture(Source, texcoord + 0.50*offsetx).xyz;
   vec3 l4  = texture(Source, texcoord - 0.50*offsetx).xyz;
   vec3 ref = texture(Source, texcoord).xyz;

   float lum1 = min(texture(Source, vTexCoord - dx).a, texture(Source, vTexCoord + dx).a);
   float lum2 = max(ref.x, 0.0);

   float dif = max(max(abs(l1.x-l2.x), abs(l1.y-l2.y)), max(abs(l1.z-l2.z), abs(l1.x*l1.x-l2.x*l2.x)));
   float dff = max(max(abs(l3.x-l4.x), abs(l3.y-l4.y)), max(abs(l3.z-l4.z), abs(l3.x*l3.x-l4.x*l4.x)));

   float lc = (1.0-smoothstep(0.10, 0.20, abs(lum2-lum1)))*pow(dff, 0.125);

   float sweight = smoothstep(0.05-0.03*lc, 0.45 - 0.40*lc, dif);

   vec3 signal = ref;

   if (abs(global.ntsc_sharp) > -0.1)
   {
	  float lummix = mix(lum2, lum1, 0.1*abs(global.ntsc_sharp));
      float lm1 =  mix(lum2*lum2, lum1*lum1, 0.1*abs(global.ntsc_sharp)); lm1 = sqrt(lm1);
      float lm2 =  mix(sqrt(lum2), sqrt(lum1), 0.1*abs(global.ntsc_sharp)); lm2 = lm2*lm2;

	  float k1 = abs(lummix - lm1) + 0.00001;
	  float k2 = abs(lummix - lm2) + 0.00001;

	  lummix = min((k2*lm1 + k1*lm2)/(k1+k2), 1.0);

	  signal.x = mix(lum2, lummix, smoothstep(0.25, 0.4, pow(dff, 0.125)));
	  signal.x = min(signal.x, max(global.ntsc_shape*signal.x, lum2));
   }
   else    signal.x = clamp(signal.x, -1.0, 1.0);

   vec3 rgb = signal;
   if (global.ntsc_sharp < -0.1)
   {
      rgb.x = mix(ref.x, rgb.x, sweight);
   }

   rgb = clamp(yiq2rgb(rgb), 0.0, 1.0);

   if (global.blendMode < 0.5)
   {
      vec3 orig = texture(PrePass0, vTexCoord).rgb;
      rgb = normalize(rgb + 0.00001) * min(length(rgb), length(orig));
   }

   FragColor = vec4(rgb, 1.0);

	return FragColor;
}


//!HOOK MAIN
//!SAVE FASTSHARPEN
//!BIND NTSC3
//!WIDTH NTSC3.w
//!HEIGHT NTSC3.h
//!DESC CRT Guest Advanced NTSC -- NtscPass

// Parameters
#define CSHARPEN  0.0   // between 0.0 and 5.0
#define CCONTRAST 0.05  // between 0.0 and 0.25
#define CDETAILS  1.0   // between 0.0 and 1.0

// libretro <-> mpv compatibility layer
#define COMPAT_TEXTURE(c,d) texture(c,d)
#define Source NTSC3_raw
struct params_ {
	vec4 SourceSize;
} params = params_(
	vec4(NTSC3_size, NTSC3_pt)
);
#define CCONTR CCONTRAST
vec2 vTexCoord = NTSC3_pos;

vec4 hook() {
	vec4 FragColor = vec4(0.0);

	vec2 g01 = vec2(-1.0, 0.0)*params.SourceSize.zw;
	vec2 g21 = vec2( 1.0, 0.0)*params.SourceSize.zw;

	vec3 c01 = texture(Source, vTexCoord + g01).rgb;
	vec3 c21 = texture(Source, vTexCoord + g21).rgb;
	vec3 c11 = texture(Source, vTexCoord      ).rgb;

	vec3 b11 = 0.5*(c01+c21);

	float contrast = max(max(c11.r,c11.g),c11.b);
	contrast = mix(2.0*CCONTR, CCONTR, contrast);

	vec3 mn1 = min(c01,c21); mn1 = min(mn1,c11*(1.0-contrast));
	vec3 mx1 = max(c01,c21); mx1 = max(mx1,c11*(1.0+contrast));

	vec3 dif = pow(mx1-mn1+0.0001, vec3(0.75,0.75,0.75));
	vec3 sharpen = mix(vec3(CSHARPEN*CDETAILS), vec3(CSHARPEN), dif);

	c11 = clamp(mix(c11,b11,-sharpen), mn1,mx1);

	FragColor = vec4(c11,1.0);

	return FragColor;
}


// TODO missing pass "PrePass" / "stock.slang" with mipmap_input param??


//!HOOK MAIN
//!SAVE AVGLUM
//!BIND FASTSHARPEN
//!WIDTH FASTSHARPEN.w
//!HEIGHT FASTSHARPEN.h
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- AvgLumPass

// Parameters
#define BLOOM_SMOOTH 0.7  // between 0.5 and 0.99

// libretro <-> mpv compatibility layer
#define COMPAT_TEXTURE(c,d) texture(c,d)
#define Source FASTSHARPEN_raw
struct params_ {
	vec4 SourceSize;
} params = params_(
	vec4(FASTSHARPEN_size, FASTSHARPEN_pt)
);
#define SourceSize params.SourceSize
#define lsmooth BLOOM_SMOOTH
vec2 vTexCoord = FASTSHARPEN_pos * 1.00001;
#define TEX0 vTexCoord

float dist(vec3 A, vec3 B)
{
	float r = 0.5 * (A.r + B.r);
	vec3 d = A - B;
	vec3 c = vec3(2. + r, 4., 3. - r);

	return sqrt(dot(c*d, d)) / 3.;
}

vec4 hook() {
	vec4 FragColor = vec4(0.0);

	float m = max(log2(SourceSize.x), log2(SourceSize.y));
	m = floor(max(m, 1.0))-1.0;

	vec2 dx = vec2(1.0/SourceSize.x, 0.0);
	vec2 dy = vec2(0.0, 1.0/SourceSize.y);
	vec2 y2 = 2.0*dy;
	vec2 x2 = 2.0*dx;

	float ltotal = 0.0;

	ltotal+= length(textureLod(Source, vec2(0.3, 0.3), m).rgb);
	ltotal+= length(textureLod(Source, vec2(0.3, 0.7), m).rgb);
	ltotal+= length(textureLod(Source, vec2(0.7, 0.3), m).rgb);
	ltotal+= length(textureLod(Source, vec2(0.7, 0.7), m).rgb);

	ltotal*=0.25;

	ltotal = pow(0.577350269 * ltotal, 0.70);

	// TODO PassFeedpack is meant to be the output of this pass at the previous frame. Change this when mpv supports this feature
	//float lhistory = texture(AvgLumPassFeedback, vec2(0.5,0.5)).a;
	float lhistory = ltotal;

	ltotal = mix(ltotal, lhistory, lsmooth);

	vec3 l1 = COMPAT_TEXTURE(Source, TEX0.xy           ).rgb;
	vec3 r1 = COMPAT_TEXTURE(Source, TEX0.xy +dx       ).rgb;
	vec3 l2 = COMPAT_TEXTURE(Source, TEX0.xy -dx       ).rgb;
	vec3 r2 = COMPAT_TEXTURE(Source, TEX0.xy +x2       ).rgb;

	float c1 = dist(l2,l1);
	float c2 = dist(l1,r1);
	float c3 = dist(r2,r1);

	FragColor = vec4(c1,c2,c3,ltotal);

	return FragColor;
}


//!HOOK MAIN
//!SAVE INTERLACE
//!BIND MAIN
//!BIND FASTSHARPEN
//!BIND AVGLUM
//!WIDTH AVGLUM.w
//!HEIGHT AVGLUM.h
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- LinearizePass

// Parameters
#define DOWNSAMPLE_LEVEL_X     0.0    // between 0.0 and 2.0
#define DOWNSAMPLE_LEVEL_Y     0.0    // between 0.0 and 2.0
#define GAMMA_INPUT            2.0    // between 1.0 and 5.0
#define GAMMA_OUTPUT           1.95   // between 1.0 and 5.0
#define INTERLACING_MODE       1      // either 0 (off), 1-3 (normal) or 4-5 (interpolation)
#define INTERLACING_RESOLUTION 400.0  // between 0.0 and 800.0
#define INTERLACING_SATURATION 0.25   // between 0.0 and 1.0
#define INTERLACING_SCAN       0.2    // between 0.0 and 1.0
#define INTERNAL_RESOLUTION    0.0    // between 0.0 and 6.0

// libretro <-> mpv compatibility layer
#define COMPAT_TEXTURE(c,d) texture(c,d)
#define Source AVGLUM_raw
#define NtscPass FASTSHARPEN_raw
struct global_ {
	vec4 SourceSize;
} global = global_(
	vec4(AVGLUM_size, AVGLUM_pt)
);
struct params_ {
	vec4 OriginalSize;
	uint FrameCount;
} params = params_(
	vec4(MAIN_size, MAIN_pt),
	uint(frame)
);
#define downsample_levelx DOWNSAMPLE_LEVEL_X
#define downsample_levely DOWNSAMPLE_LEVEL_Y
#define gamma_out GAMMA_OUTPUT
#define interm float(INTERLACING_MODE)
#define inter INTERLACING_RESOLUTION
#define iscans INTERLACING_SATURATION
#define iscan INTERLACING_SCAN
#define intres INTERNAL_RESOLUTION
vec2 vTexCoord = AVGLUM_pos * 1.00001;

vec3 plant (vec3 tar, float r)
{
	float t = max(max(tar.r,tar.g),tar.b) + 0.00001;
	return tar * r / t;
}


vec3 fetch_pixel(vec2 coord)
{
	vec2 dx = vec2(global.SourceSize.z, 0.0) * downsample_levelx;
	vec2 dy = vec2(0.0, global.SourceSize.w) * downsample_levely;
	vec2 d1 = dx + dy;
	vec2 d2 = dx - dy;

	float sum = 15.0;
	vec3 result = 3.0*COMPAT_TEXTURE(NtscPass, coord     ).rgb +
	              2.0*COMPAT_TEXTURE(NtscPass, coord + dx).rgb +
	              2.0*COMPAT_TEXTURE(NtscPass, coord - dx).rgb +
	              2.0*COMPAT_TEXTURE(NtscPass, coord + dy).rgb +
	              2.0*COMPAT_TEXTURE(NtscPass, coord - dy).rgb +
	              COMPAT_TEXTURE(NtscPass, coord + d1).rgb +
	              COMPAT_TEXTURE(NtscPass, coord - d1).rgb +
	              COMPAT_TEXTURE(NtscPass, coord + d2).rgb +
	              COMPAT_TEXTURE(NtscPass, coord - d2).rgb;

	return result/sum;
}

vec4 hook() {
	vec4 FragColor = vec4(0.0);

	vec3 c1 = COMPAT_TEXTURE(NtscPass, vTexCoord).rgb;
	vec3 c2 = COMPAT_TEXTURE(NtscPass, vTexCoord + vec2(0.0, params.OriginalSize.w)).rgb;

	if ((downsample_levelx + downsample_levely) > 0.025)
	{
		c1 = fetch_pixel(vTexCoord);
		c2 = fetch_pixel(vTexCoord + vec2(0.0, params.OriginalSize.w));
	}

	vec3  c  = c1;

	float intera = 1.0;
	float gamma_in = clamp(GAMMA_INPUT, 1.0, 5.0);

	float m1 = max(max(c1.r,c1.g),c1.b);
	float m2 = max(max(c2.r,c2.g),c2.b);
	vec3 df = abs(c1-c2);

	float d = max(max(df.r,df.g),df.b);
	if (interm == 2.0) d = mix(0.1*d,10.0*d, step(m1/(m2+0.0001),m2/(m1+0.0001)));

	float r = m1;

	float yres_div = 1.0; if (intres > 1.25) yres_div = intres;

	if (inter <= params.OriginalSize.y/yres_div && interm > 0.5 && intres != 1.0 && intres != 0.5)
	{
		intera = 0.25;
		float line_no  = clamp(floor(mod(params.OriginalSize.y*vTexCoord.y, 2.0)), 0.0, 1.0);
		float frame_no = clamp(floor(mod(float(params.FrameCount),2.0)), 0.0, 1.0);
		float ii = abs(line_no-frame_no);

		if (interm < 3.5)
		{
			c2 = plant(mix(c2, c2*c2, iscans), max(max(c2.r,c2.g),c2.b));
			r = clamp(max(m1*ii, (1.0-iscan)*min(m1,m2)), 0.0, 1.0);
			c = plant( mix(mix(c1,c2, min(mix(m1, 1.0-m2, min(m1,1.0-m1))/(d+0.00001),1.0)), c1, ii), r);
			if (interm == 3.0) c = (1.0-0.5*iscan)*mix(c2, c1, ii);
		}
		if (interm == 4.0) { c = plant(mix(c, c*c, 0.5*iscans), max(max(c.r,c.g),c.b)) * (1.0-0.5*iscan); }
		if (interm == 5.0) { c = mix(c2, c1, 0.5); c = plant(mix(c, c*c, 0.5*iscans), max(max(c.r,c.g),c.b)) * (1.0-0.5*iscan); }
	}
	c = pow(c, vec3(gamma_in));

	if (vTexCoord.x > 0.5) gamma_in = intera; else gamma_in = 1.0/gamma_in;

	FragColor = vec4(c, gamma_in);

	return FragColor;
}


//!HOOK MAIN
//!SAVE NTSCA1
//!BIND MAIN
//!BIND INTERLACE
//!WIDTH OUTPUT.w
//!HEIGHT INTERLACE.h
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- Pass1

// Parameters
#define HORIZONTAL_FILTER_RANGE          1.75  // between 1.0 and 8.0
#define HORIZONTAL_BLUR_SIGMA            0.85  // between 0.1 and 7.0
#define SUBSTRACIVE_SHARPNESS            1.4   // between 0.0 and 3.0
#define SHARPNESS_DEFINITION             1.2   // between 0.0 and 2.0
#define MAXIMUM_SHARPNESS                0.18  // between 0.0 and 0.3
#define SUBSTRACTIVRE_SHARPNESS_RINGING  0.4   // between 0.0 and 4.0
#define SCANLINE_SPIKE_REMOVAL           1.0   // between 0.0 and 2.0

// libretro <-> mpv compatibility layer
#define COMPAT_TEXTURE(c,d) texture(c,d)
#define LinearizePass INTERLACE_raw
struct params_ {
	vec4 OriginalSize;
} params = params_(
	vec4(MAIN_size, MAIN_pt)
);
#define HSHARPNESS HORIZONTAL_FILTER_RANGE
#define SIGMA_HOR HORIZONTAL_BLUR_SIGMA
#define S_SHARP SUBSTRACIVE_SHARPNESS
#define HSHARP SHARPNESS_DEFINITION
#define MAXS MAXIMUM_SHARPNESS
#define HARNG SUBSTRACTIVRE_SHARPNESS_RINGING
#define spike SCANLINE_SPIKE_REMOVAL
vec2 vTexCoord = INTERLACE_pos * 1.00001;

float invsqrsigma = 1.0/(2.0*SIGMA_HOR*SIGMA_HOR);

float gaussian(float x)
{
	return exp(-x*x*invsqrsigma);
}

vec4 hook() {
	vec4 FragColor = vec4(0.0);

	vec2 prescalex = vec2(textureSize(LinearizePass, 0)) / params.OriginalSize.xy;

	vec4 SourceSize = params.OriginalSize * vec4(2.0*prescalex.x, prescalex.y, 0.5/prescalex.x, 1.0/prescalex.y);

	float f = fract(SourceSize.x * vTexCoord.x);
	f = 0.5 - f;
	vec2 tex = floor(SourceSize.xy * vTexCoord)*SourceSize.zw + 0.5*SourceSize.zw;
	vec3 color = 0.0.xxx;
	float scolor = 0.0;
	vec2 dx  = vec2(SourceSize.z, 0.0);

	float w = 0.0;
	float swsum = 0.0;
	float wsum = 0.0;
	vec3 pixel;

	float hsharpness = HSHARPNESS;
	vec3 cmax = 0.0.xxx;
	vec3 cmin = 1.0.xxx;
	float sharp = gaussian(hsharpness) * S_SHARP;
	float maxsharp = MAXS;
	float FPR = hsharpness;
	float fpx = 0.0;
	float sp = 0.0;
	float sw = 0.0;

	float ts = 0.025;
	vec3 luma = vec3(0.2126, 0.7152, 0.0722);

	float LOOPSIZE = ceil(2.0*FPR);
	float CLAMPSIZE = round(2.0*LOOPSIZE/3.0);

	float n = -LOOPSIZE;

	do
	{
		pixel  = COMPAT_TEXTURE(LinearizePass, tex + n*dx).rgb;
		sp = max(max(pixel.r,pixel.g),pixel.b);

		w = gaussian(n+f) - sharp;
		fpx = abs(n+f-sign(n)*FPR)/FPR;
		if (abs(n) <= CLAMPSIZE) { cmax = max(cmax, pixel); cmin = min(cmin, pixel); }
		if (w < 0.0) w = clamp(w, mix(-maxsharp, 0.0, pow(clamp(fpx,0.0,1.0), HSHARP)), 0.0);

		color = color + w * pixel;
		wsum  = wsum + w;

		sw = max(w, 0.0) * (dot(pixel,luma) + ts);
		scolor = scolor + sw * sp;
		swsum = swsum + sw;

		n = n + 1.0;

	} while (n <= LOOPSIZE);

	color = color / wsum;
	scolor = scolor / swsum;

	color = clamp(mix(clamp(color, cmin, cmax), color, HARNG), 0.0, 1.0);

	scolor = clamp(mix(max(max(color.r, color.g),color.b), scolor, spike), 0.0, 1.0);

	FragColor = vec4(color, scolor);

	return FragColor;
}

//!HOOK MAIN
//!SAVE HBLUR
//!BIND MAIN
//!BIND INTERLACE
//!WIDTH 800.0
//!HEIGHT INTERLACE.h
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- Horizontal gaussian blur

// Parameters
#define M_GLOW        false  // boolean
#define M_GLOW_CUTOFF 0.12   // between 0.0 and 0.4
#define M_GLOW_LOW    0.35   // between 0.0 and 7.0
#define M_GLOW_HIGH   5.0    // between 0.0 and 7.0
#define M_GLOW_DIST   1.0    // between 0.2 and 4.0
#define M_GLOW_MASK   1.0    // between 0.0 and 2.0
#define SIZEH         6.0    // between 1.0 and 50.0
#define SIGMA_H       1.2    // between 0.2 and 15.0

float gaussian(float x) {
	const float invsqrsigma = 1.0/(2.0*SIGMA_H*SIGMA_H);
	return exp(-x*x*invsqrsigma);
}

vec3 plant (vec3 tar, float r) {
	float t = max(max(tar.r,tar.g),tar.b) + 0.00001;
	return tar * r / t;
}

vec4 hook() {
	vec4 SourceSize1 = vec4(MAIN_size, 1.0 / MAIN_size.x, 1.0 / MAIN_size.y);
	float f = fract(SourceSize1.x * INTERLACE_pos.x);
	f = 0.5 - f;
	vec2 tex = floor(SourceSize1.xy * INTERLACE_pos)*SourceSize1.zw + 0.5*SourceSize1.zw;
	vec3 color = vec3(0.0);
	vec2 dx  = vec2(SourceSize1.z, 0.0);

	float w;
	float wsum = 0.0;
	vec3 pixel;
	float n = -SIZEH;

	do
	{
		pixel  = INTERLACE_tex(tex + n*dx).rgb;
		if (M_GLOW)
		{
			pixel = max(pixel-M_GLOW_CUTOFF, 0.0);
			pixel = plant(pixel, max(max(max(pixel.r,pixel.g),pixel.b)-M_GLOW_CUTOFF,0.0));
		}
		w      = gaussian(n+f);
		color  = color + w * pixel;
		wsum   = wsum + w;
		n = n + 1.0;

	} while (n <= SIZEH);

	color = color / wsum;

	return vec4(color, 1.0);
}

//!HOOK MAIN
//!SAVE VBLUR
//!BIND MAIN
//!BIND HBLUR
//!WIDTH 800.0
//!HEIGHT 600.0
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- Vertical gaussian blur

// Parameters
#define SIZEV   6.0  // between 1.0 and 50.0
#define SIGMA_V 1.2  // between 0.2 and 15.0

float gaussian(float x) {
	const float invsqrsigma = 1.0/(2.0*SIGMA_V*SIGMA_V);
	return exp(-x*x*invsqrsigma);
}

vec4 hook() {
	vec4 SourceSize1 = vec4(HBLUR_size.x, MAIN_size.y, 1.0 / HBLUR_size.x, 1.0 / MAIN_size.y);
	float f = fract(SourceSize1.x * HBLUR_pos.x);
	f = 0.5 - f;
	vec2 tex = floor(SourceSize1.xy * HBLUR_pos)*SourceSize1.zw + 0.5*SourceSize1.zw;
	vec3 color = vec3(0.0);
	vec2 dy  = vec2(0.0, SourceSize1.w);

	float w;
	float wsum = 0.0;
	vec3 pixel;
	float n = -SIZEV;

	do
	{
		pixel  = HBLUR_tex(tex + n*dy).rgb;
		w      = gaussian(n+f);
		color  = color + w * pixel;
		wsum   = wsum + w;
		n = n + 1.0;

	} while (n <= SIZEV);

	color = color / wsum;

	return vec4(color, 1.0);
}

//!HOOK MAIN
//!SAVE HBLOOM
//!BIND MAIN
//!BIND INTERLACE
//!BIND VBLUR
//!WIDTH 800.0
//!HEIGHT 600.0
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- Horizontal bloom

// Parameters
#define FINE_BLOOM   1.0   // between 1.0 and 4.0
#define SIZEHB       3.0   // between 1.0 and 50.0
#define SIGMA_HB     0.75  // between 0.25 and 15.0
#define BLOOMCUT_H   0.0   // between -0.5 and 0.5

float gaussian(float x) {
	const float invsqrsigma = 1.0/(2.0*SIGMA_HB*SIGMA_HB);
	return exp(-x*x*invsqrsigma);
}

vec4 hook() {
	vec4 SourceSize1 = vec4(MAIN_size, 1.0 / MAIN_size.x, 1.0 / MAIN_size.y) * mix(1.0.xxxx, vec4(FINE_BLOOM, FINE_BLOOM, 1.0/FINE_BLOOM, 1.0/FINE_BLOOM), min(FINE_BLOOM-1.0,1.0));
	float f = fract(SourceSize1.x * VBLUR_pos.x);
	f = 0.5 - f;
	vec2 tex = floor(SourceSize1.xy * VBLUR_pos)*SourceSize1.zw + 0.5*SourceSize1.zw;
	vec4 color = vec4(0.0);
	vec2 dx  = vec2(SourceSize1.z, 0.0);

	float w;
	float wsum = 0.0;
	vec4 pixel;
	float n = -SIZEHB;

	do
	{
		pixel  = INTERLACE_tex(tex + n*dx);
		w = gaussian(n+f);
		w      = (BLOOMCUT_H >= 0.0) ? max(w - BLOOMCUT_H, 0.0) : (max(w + BLOOMCUT_H, 0.0)/(1.0 + BLOOMCUT_H));
		pixel.a = max(max(pixel.r, pixel.g),pixel.b);
		pixel.a*=pixel.a*pixel.a;
		color  = color + w * pixel;
		wsum   = wsum + w;
		n = n + 1.0;

	} while (n <= SIZEHB);

	color = color / wsum;

	return vec4(color.rgb, pow(color.a, 0.333333));
}

//!HOOK MAIN
//!SAVE VBLOOM
//!BIND MAIN
//!BIND INTERLACE
//!BIND HBLOOM
//!WIDTH 800.0
//!HEIGHT 600.0
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- Vertical bloom

// Parameters
#define FINE_BLOOM   1.0  // between 1.0 and 4.0
#define SIZEVB       3.0  // between 1.0 and 50.0
#define SIGMA_VB     0.6  // between 0.25 and 15.0
#define BLOOMCUT_V   0.0  // between -0.5 and 0.5

float gaussian(float x) {
	const float invsqrsigma = 1.0/(2.0*SIGMA_VB*SIGMA_VB);
	return exp(-x*x*invsqrsigma);
}

vec4 hook() {
	vec4 SourceSize1 = vec4(HBLOOM_size, 1.0 / MAIN_size.x, 1.0 / MAIN_size.y);
	SourceSize1 = SourceSize1 * mix(1.0.xxxx, vec4(FINE_BLOOM, FINE_BLOOM, 1.0/FINE_BLOOM, 1.0/FINE_BLOOM), min(FINE_BLOOM-1.0,1.0));

	float f = fract(SourceSize1.y * HBLOOM_pos.y);
	f = 0.5 - f;
	vec2 tex = floor(SourceSize1.xy * HBLOOM_pos)*SourceSize1.zw + 0.5*SourceSize1.zw;
	vec4 color = vec4(0.0);
	vec2 dy  = vec2(0.0, SourceSize1.w);

	float w;
	float wsum = 0.0;
	vec4 pixel;
	float n = -SIZEVB;

	do
	{
		pixel  = HBLOOM_tex(tex + n*dy);
		w = gaussian(n+f);
		w      = (BLOOMCUT_V >= 0.0) ? max(w - BLOOMCUT_V, 0.0) : (max(w + BLOOMCUT_V, 0.0)/(1.0 + BLOOMCUT_V));
		pixel.a*=pixel.a*pixel.a;
		color  = color + w * pixel;
		wsum   = wsum + w;
		n = n + 1.0;

	} while (n <= SIZEVB);

	color = color / wsum;

	return vec4(color.rgb, pow(color.a, 0.175));
}

//!HOOK MAIN
//!SAVE NTSCA2
//!BIND MAIN
//!BIND AVGLUM
//!BIND INTERLACE
//!BIND NTSCA1
//!BIND VBLOOM
//!WIDTH OUTPUT.w
//!HEIGHT OUTPUT.h
//!COMPONENTS 4
//!DESC CRT Guest Advanced NTSC -- Second advanced pass

// Parameters
#define glow  0.08 // -2.0 2.0 0.01
#define bloom  0.0 // -2.0 2.0 0.05
#define mask_bloom  0.0 // -2.0 2.0 0.05
#define bloom_dist  0.0 // -2.0 3.0 0.05
#define halation  0.0 // -2.0 2.0 0.025
#define bmask1  0.0 // -1.0 1.0 0.025
#define hmask1  0.5 // 0.0 1.0 0.025
#define gamma_c  1.0 // 0.50 2.0 0.025
#define brightboost  1.40 // 0.25 10.0 0.05
#define brightboost1  1.10 // 0.25 3.00 0.025
#define clips  0.0 // -1.0 1.0 0.05
#define gsl  0.0 // -1.0 2.0 1.0
#define scanline1  6.0 // -20.0 40.0 0.5
#define scanline2  8.0 // 0.0 70.0 1.0
#define beam_min  1.30 // 0.25 10.0 0.05
#define beam_max  1.00 // 0.2 3.5 0.025
#define tds  0.0 // 0. 1.0 1.0
#define beam_size  0.60 // 0.0 1.0 0.05
#define scans  0.50 // -5.0 5.0 0.10
#define scan_falloff  1.0 // 0.10 2.0 0.025
#define ssharp  0.0 // 0.0 0.30 0.01
#define scangamma  2.40 // 0.5 5.0 0.05
#define no_scanlines  0.0 // 0.0 1.5 0.05
#define intres  0.0 // 0.0 6.0 0.5 // Joint parameter with linearize pass, values must match
#define IOS  0.0 // 0.0 4.0 1.0
#define OS  1.0 // 0.0 2.0 1.0
#define BLOOM  0.0 // 0.0 20.0 1.0
#define warpX  0.0 // 0.0 0.25 0.01
#define warpY  0.0 // 0.0 0.25 0.01
#define c_shape  0.25 // 0.05 0.60 0.05
#define overscanX  0.0 // -200.0 200.0 1.0
#define overscanY  0.0 // -200.0 200.0 1.0

#define eps 1e-8

float st(float x) {
	return exp2(-10.0*x*x);
}

float st1(float x) {
	return exp2(-8.0*x*x);
}

float sw0(float x, float color, float scanline) {
	float tmp = mix(beam_min, beam_max, color);
	float ex = x*tmp;
	ex = (gsl > -0.5) ? ex*ex : mix(ex*ex, ex*ex*ex, 0.4);
	return exp2(-scanline*ex);
}

float sw1(float x, float color, float scanline) {
	x = mix (x, beam_min*x, max(x-0.4*color,0.0));
	float tmp = mix(1.2*beam_min, beam_max, color);
	float ex = x*tmp;
	return exp2(-scanline*ex*ex);
}

float sw2(float x, float color, float scanline) {
	float tmp = mix((2.5-0.5*color)*beam_min, beam_max, color);
	tmp = mix(beam_max, tmp, pow(x, color+0.3));
	float ex = x*tmp;
	return exp2(-scanline*ex*ex);
}


vec2 Warp(vec2 pos) {
	pos  = pos*2.0-1.0;
	pos  = mix(pos, vec2(pos.x*inversesqrt(1.0-c_shape*pos.y*pos.y), pos.y*inversesqrt(1.0-c_shape*pos.x*pos.x)), vec2(warpX, warpY)/c_shape);
	return pos*0.5 + 0.5;
}

vec2 Overscan(vec2 pos, float dx, float dy){
	pos=pos*2.0-1.0;
	pos*=vec2(dx,dy);
	return pos*0.5+0.5;
}


vec3 gc(vec3 c) {
	float mc = max(max(c.r,c.g),c.b);
	float mg = pow(mc, 1.0/gamma_c);
	return c * mg/(mc + eps);
}

vec3 plant (vec3 tar, float r) {
	float t = max(max(tar.r,tar.g),tar.b) + 0.00001;
	return tar * r / t;
}

vec4 hook() {
	float prescalex = float(textureSize(INTERLACE_raw, 0).x) / (2.0*MAIN_size.x);

	vec4 SourceSize = vec4(MAIN_size, 1.0 / MAIN_size.x, 1.0 / MAIN_size.y)
		* vec4(prescalex, 1.0, 1.0/prescalex, 1.0);

	SourceSize*= vec4(2.0, 1.0, 0.5, 1.0);

	float lum = AVGLUM_tex(vec2(0.5,0.5)).a;

	float gamma_in = 1.0/ INTERLACE_tex(vec2(0.25,0.25)).a;
	float intera = INTERLACE_tex(vec2(0.75,0.25)).a;
	bool interb  = ((intera < 0.5) || (no_scanlines > 0.025));

	float SourceY = SourceSize.y;
	float sy = 1.0;
	if (intres == 1.0) sy = SourceY/224.0;
	if (intres > 0.25 && intres != 1.0) sy = intres;
	SourceSize*=vec4(1.0, 1.0/sy, 1.0, sy);

	// Calculating texel coordinates

	vec2 texcoord = VBLOOM_pos;
	if (IOS > 0.0 && !interb){
		vec2 ofactor = target_size / MAIN_size;
		vec2 intfactor = (IOS < 2.5) ? floor(ofactor) : ceil(ofactor);
		vec2 diff = ofactor/intfactor;
		float scan = diff.y;
		texcoord = Overscan(texcoord, scan, scan);
		if (IOS == 1.0 || IOS == 3.0) texcoord = vec2(VBLOOM_pos.x, texcoord.y);
	}

	float factor  = 1.00 + (1.0-0.5*OS)*BLOOM/100.0 - lum*BLOOM/100.0;
	texcoord  = Overscan(texcoord, factor, factor);

	texcoord = Overscan(
		texcoord,
		(MAIN_size.x - overscanX) / MAIN_size.x,
		(MAIN_size.y - overscanY) / MAIN_size.y);

	vec2 pos  = Warp(texcoord);
	vec2 pos0 = Warp(VBLOOM_pos);

	vec2 coffset = vec2(0.5, 0.5);

	vec2 ps = SourceSize.zw;
	vec2 OGL2Pos = pos * SourceSize.xy - coffset;
	vec2 fp = fract(OGL2Pos);

	vec2 dx = vec2(ps.x,0.0);
	vec2 dy = vec2(0.0, ps.y);

	// Reading the texels

	float  f = fp.y;

	vec2 pC4 = floor(OGL2Pos) * ps + 0.5*ps;
	pC4.x = pos.x;

	if (intres == 0.5 && prescalex < 1.5) {
		pC4.y = floor(pC4.y * MAIN_size.y)/MAIN_size.y + 0.5/MAIN_size.y;
	}

	if (interb && no_scanlines < 0.025) {
		pC4.y = pos.y;
	} else if (interb) {
		pC4.y = pC4.y + smoothstep(0.40-0.5*no_scanlines, 0.60 + 0.5*no_scanlines, f)*SourceSize.w;
	}

	vec3 color1 = NTSCA1_tex(pC4).rgb;
	vec3 scolor1 = NTSCA1_tex(pC4).aaa;

	if(!interb) color1 = pow(color1, vec3(scangamma/gamma_in));

	pC4+=dy;

	if (intres == 0.5 && prescalex < 1.5) {
		pC4.y = floor((pos.y + 0.33*dy.y) * MAIN_size.y) / MAIN_size.y + 0.5 / MAIN_size.y;
	}

	vec3 color2 = NTSCA1_tex(pC4).rgb;
	vec3 scolor2 = NTSCA1_tex(pC4).aaa;

	if(!interb) color2 = pow(color2, vec3(scangamma/gamma_in));

	vec3 ctmp = color1; float w3 = 1.0; vec3 color = color1;
	vec3 one = vec3(1.0);

	if (!interb) {
		// calculating scanlines
		vec3 luma = vec3(0.2126, 0.7152, 0.0722);
		float ssub = ssharp*max(abs(scolor1.x-scolor2.x), abs(dot(color1,luma)-dot(color2,luma)));

		float shape1 = mix(scanline1, scanline2 + ssub * scolor1.x * 35.0, f);
		float shape2 = mix(scanline1, scanline2 + ssub * scolor2.x * 35.0, 1.0-f);

		float wt1 = st(f);
		float wt2 = st(1.0-f);

		vec3 color00 = color1*wt1 + color2*wt2;
		vec3 scolor0 = scolor1*wt1 + scolor2*wt2;

		ctmp = color00/(wt1+wt2);
		vec3 sctmp = scolor0/(wt1+wt2);

		float wf1, wf2;

		vec3 cref1 = mix(sctmp, scolor1, beam_size); float creff1 = pow(max(max(cref1.r,cref1.g),cref1.b), scan_falloff);
		vec3 cref2 = mix(sctmp, scolor2, beam_size); float creff2 = pow(max(max(cref2.r,cref2.g),cref2.b), scan_falloff);

		if (tds > 0.5) { shape1 = mix(scanline2, shape1, creff1); shape2 = mix(scanline2, shape2, creff2); }

		float f1 = f;
		float f2 = 1.0-f;

		if (gsl <  0.5) { wf1 = sw0(f1,creff1,shape1); wf2 = sw0(f2,creff2,shape2);} else
		if (gsl == 1.0) { wf1 = sw1(f1,creff1,shape1); wf2 = sw1(f2,creff2,shape2);} else
		{ wf1 = sw2(f1,creff1,shape1); wf2 = sw2(f2,creff2,shape2);}

		if ((wf1 + wf2) > 1.0) { float wtmp = 1.0/(wf1+wf2); wf1*=wtmp; wf2*=wtmp; }

		// Scanline saturation application

		vec3 w1 = vec3(wf1); vec3 w2 = vec3(wf2);
		w3 = wf1+wf2;

		float mc1 = max(max(color1.r,color1.g),color1.b) + eps;
		float mc2 = max(max(color2.r,color2.g),color2.b) + eps;

		cref1 = color1 / mc1;
		cref2 = color2 / mc2;

		float scanpow1 = (scans > 0.0) ? 1.0 : pow(f1, 0.375);
		float scanpow2 = (scans > 0.0) ? 1.0 : pow(f2, 0.375);
		w1 = pow(w1, mix(2.0*abs(scans).xxx + 1.0, 1.0.xxx, mix(1.0.xxx, cref1, scanpow1)));
		w2 = pow(w2, mix(2.0*abs(scans).xxx + 1.0, 1.0.xxx, mix(1.0.xxx, cref2, scanpow2)));

		if (abs(clips) > 0.005)
		{
			sy = mc1; vec3 l1 = sqrt(w1*wt1); vec3 l2 = sqrt(w2*wt2);
			one = (clips > 0.0) ? w1 : mix(w1, l1, sy);
			float sat = 1.0001-min(min(cref1.r,cref1.g),cref1.b);
			color1 = mix(color1, plant(pow(color1, 0.70.xxx-0.325*sat),sy), pow(sat,0.3333)*one*abs(clips));
			sy = mc2;
			sat =       1.0001-min(min(cref2.r,cref2.g),cref2.b);
			one = (clips > 0.0) ? w2 : mix(w2, l2, sy);
			color2 = mix(color2, plant(pow(color2, 0.70.xxx-0.325*sat),sy), pow(sat,0.3333)*one*abs(clips));
		}

		color = (gc(color1)*w1 + gc(color2)*w2);

		color = min(color, 1.0);
	}

	if (interb) {
		color = gc(color1);
	}

	float colmx = max(max(ctmp.r,ctmp.g),ctmp.b);

	if(!interb) color = pow( color, vec3(gamma_in/scangamma) );

	return  vec4(color, colmx);
}

//!HOOK MAIN
//!SAVE MAIN
//!BIND MAIN
//!BIND AFTERGLOW
//!BIND AVGLUM
//!BIND INTERLACE
//!BIND NTSCA1
//!BIND VBLUR
//!BIND VBLOOM
//!BIND NTSCA2
//!WIDTH OUTPUT.w
//!HEIGHT OUTPUT.h
//!COMPONENTS 3
//!DESC CRT Guest Advanced NTSC -- Deconvergence

// Parameters
#define no_scanlines   0.0 // 0.0 1.5 0.05
#define m_glow   0.0 // 0.0 1.0 1.0
#define m_glow_low   0.35 // 0.0 7.0 0.05
#define m_glow_high   5.0 // 0.0 7.0 0.10
#define m_glow_dist   1.0 // 0.20 4.0 0.05
#define m_glow_mask   1.0 // 0.0 2.0 0.025
#define glow   0.08 // -2.0 2.0 0.01
#define bloom   0.0 // -2.0 2.0 0.05
#define mask_bloom   0.0 // -2.0 2.0 0.05
#define bloom_dist   0.0 // -2.0 3.0 0.05
#define halation   0.0 // -2.0 2.0 0.025
#define bmask1   0.0 // -1.0 1.0 0.025
#define hmask1   0.5 // 0.0 1.0 0.025
#define brightboost   1.40 // 0.25 10.0 0.05
#define brightboost1   1.10 // 0.25 3.00 0.025
#define gamma_c   1.0 // 0.50 2.0 0.025
#define IOS   0.0 // 0.0 4.0 1.0
#define OS   1.0 // 0.0 2.0 1.0
#define BLOOM   0.0 // 0.0 20.0 1.0
#define csize   0.0 // 0.0 0.25 0.005
#define bsize1   0.01 // 0.0 3.0 0.01
#define sborder   0.75 // 0.25 2.0 0.05
#define barspeed   50.0 // 5.0 200.0 1.0
#define barintensity   0.0 // -1.0 1.0 0.01
#define bardir   0.0 // 0.0 1.0 1.0
#define warpX   0.0 // 0.0 0.25 0.01
#define warpY   0.0 // 0.0 0.25 0.01
#define c_shape   0.25 // 0.05 0.60 0.05
#define overscanX   0.0 // -200.0 200.0 1.0
#define overscanY   0.0 // -200.0 200.0 1.0
#define shadowMask   0.0 // -1.0 13.0 1.0
#define maskstr   0.3 // -0.5 1.0 0.025
#define mcut   1.10 // 0.0 2.0 0.05
#define maskboost   1.0 // 1.0 3.0 0.05
#define masksize   1.0 // 1.0 4.0 1.0
#define mask_zoom   0.0 // -5.0 5.0 1.0
#define mshift   0.0 // 0.0 1.0 0.5
#define mask_layout   0.0 // 0.0 1.0 1.0
#define maskDark   0.5 // 0.0 2.0 0.05
#define maskLight   1.5 // 0.0 2.0 0.05
#define mask_gamma   2.40 // 1.0 5.0 0.05
#define slotmask   0.0 // 0.0 1.0 0.05
#define slotmask1   0.0 // 0.0 1.0 0.05
#define slotwidth   0.0 // 0.0 16.0 1.0
#define double_slot   2.0 // 1.0 4.0 1.0
#define slotms   1.0 // 1.0 4.0 1.0
#define smoothmask   0.0 // 0.0 1.0 1.0
#define smask_mit   0.0 // 0.0 1.0 0.05
#define bmask   0.0 // 0.0 0.25 0.01
#define gamma_out   1.95 // 1.0 5.0 0.05
#define bogus_deconvergence11   0.0 // 0.0 1.0 1.0
#define dctypex   0.0 // 0.0 0.75 0.05
#define dctypey   0.0 // 0.0 0.75 0.05
#define deconrr   0.0 // -15.0 15.0 0.25
#define deconrg   0.0 // -15.0 15.0 0.25
#define deconrb   0.0 // -15.0 15.0 0.25
#define deconrry   0.0 // -15.0 15.0 0.25
#define deconrgy   0.0 // -15.0 15.0 0.25
#define deconrby   0.0 // -15.0 15.0 0.25
#define decons   1.0 // 0.0 3.0 0.10
#define addnoised   0.0 // -1.0 1.0 0.02
#define noiseresd   2.0 // 1.0 10.0 1.0
#define noisetype   0.0 // 0.0 1.0 1.0
#define post_br   1.0 // 0.25 5.0 0.01

#ifndef delinearize
// Implementation from mpv's gpu-next
vec4 delinearize(vec4 color) {
       const float _const_0_2 = 0.05958483740687370300;
       const float _const_1_2 = 1.14901518821716308593;
       color.rgb = max(color.rgb, 0.0);
       color.rgb = pow(_const_1_2 * color.rgb, vec3(1.0/2.4)) - vec3(_const_0_2);
       return color;
}
#endif

// Shadow mask (1-4 from PD CRT Lottes shader).

vec3 Mask(vec2 pos, float mx, float mb)
{
	vec3 mask = vec3(maskDark, maskDark, maskDark);
	vec3 one = vec3(1.0);

	if (shadowMask == 0.0)
	{
		float mc = 1.0 - max(maskstr, 0.0);
		pos.x = fract(pos.x*0.5);
		if (pos.x < 0.49) { mask.r = 1.0; mask.g = mc; mask.b = 1.0; }
		else { mask.r = mc; mask.g = 1.0; mask.b = mc; }
	}

	// Very compressed TV style shadow mask.
	else if (shadowMask == 1.0)
	{
		float line = maskLight;
		float odd  = 0.0;

		if (fract(pos.x/6.0) < 0.49)
			odd = 1.0;
		if (fract((pos.y + odd)/2.0) < 0.49)
			line = maskDark;

		pos.x = floor(mod(pos.x,3.0));

		if      (pos.x < 0.5) mask.r = maskLight;
		else if (pos.x < 1.5) mask.g = maskLight;
		else                  mask.b = maskLight;

		mask*=line;
	}

	// Aperture-grille.
	else if (shadowMask == 2.0)
	{
		pos.x = floor(mod(pos.x,3.0));

		if      (pos.x < 0.5) mask.r = maskLight;
		else if (pos.x < 1.5) mask.g = maskLight;
		else                  mask.b = maskLight;
	}

	// Stretched VGA style shadow mask (same as prior shaders).
	else if (shadowMask == 3.0)
	{
		pos.x += pos.y*3.0;
		pos.x  = fract(pos.x/6.0);

		if      (pos.x < 0.3) mask.r = maskLight;
		else if (pos.x < 0.6) mask.g = maskLight;
		else                  mask.b = maskLight;
	}

	// VGA style shadow mask.
	else if (shadowMask == 4.0)
	{
		pos.xy = floor(pos.xy*vec2(1.0, 0.5));
		pos.x += pos.y*3.0;
		pos.x  = fract(pos.x/6.0);

		if      (pos.x < 0.3) mask.r = maskLight;
		else if (pos.x < 0.6) mask.g = maskLight;
		else                  mask.b = maskLight;
	}

	// Trinitron mask 5
	else if (shadowMask == 5.0)
	{
		mask = vec3(0.0);
		pos.x = fract(pos.x/2.0);
		if  (pos.x < 0.49)
		{	mask.r  = 1.0;
			mask.b  = 1.0;
		}
		else     mask.g = 1.0;
		mask = clamp(mix( mix(one, mask, mcut), mix(one, mask, maskstr), mx), 0.0, 1.0);
	}

	// Trinitron mask 6
	else if (shadowMask == 6.0)
	{
		mask = vec3(0.0);
		pos.x = floor(mod(pos.x,3.0));
		if      (pos.x < 0.5) mask.r = 1.0;
		else if (pos.x < 1.5) mask.g = 1.0;
		else                    mask.b = 1.0;
		mask = clamp(mix( mix(one, mask, mcut), mix(one, mask, maskstr), mx), 0.0, 1.0);
	}

	// BW Trinitron mask 7
	else if (shadowMask == 7.0)
	{
		mask = vec3(0.0);
		pos.x = fract(pos.x/2.0);
		if  (pos.x < 0.49)
		{	mask  = 0.0.xxx;
		}
		else     mask = 1.0.xxx;
		mask = clamp(mix( mix(one, mask, mcut), mix(one, mask, maskstr), mx), 0.0, 1.0);
	}

	// BW Trinitron mask 8
	else if (shadowMask == 8.0)
	{
		mask = vec3(0.0);
		pos.x = fract(pos.x/3.0);
		if      (pos.x < 0.3) mask = 0.0.xxx;
		else if (pos.x < 0.6) mask = 1.0.xxx;
		else                  mask = 1.0.xxx;
		mask = clamp(mix( mix(one, mask, mcut), mix(one, mask, maskstr), mx), 0.0, 1.0);
	}

	// Magenta - Green - Black mask
	else if (shadowMask == 9.0)
	{
		mask = vec3(0.0);
		pos.x = fract(pos.x/3.0);
		if      (pos.x < 0.3) mask    = 0.0.xxx;
		else if (pos.x < 0.6) mask.rb = 1.0.xx;
		else                  mask.g  = 1.0;
		mask = clamp(mix( mix(one, mask, mcut), mix(one, mask, maskstr), mx), 0.0, 1.0);
	}

	// RGBX
	else if (shadowMask == 10.0)
	{
		mask = vec3(0.0);
		pos.x = fract(pos.x * 0.25);
		if      (pos.x < 0.2)  mask  = 0.0.xxx;
		else if (pos.x < 0.4)  mask.r = 1.0;
		else if (pos.x < 0.7)  mask.g = 1.0;
		else                   mask.b = 1.0;
		mask = clamp(mix( mix(one, mask, mcut), mix(one, mask, maskstr), mx), 0.0, 1.0);
	}

	// 4k mask
	else if (shadowMask == 11.0)
	{
		mask = vec3(0.0);
		pos.x = fract(pos.x * 0.25);
		if      (pos.x < 0.2)  mask.r  = 1.0;
		else if (pos.x < 0.4)  mask.rg = 1.0.xx;
		else if (pos.x < 0.7)  mask.gb = 1.0.xx;
		else                   mask.b  = 1.0;
		mask = clamp(mix( mix(one, mask, mcut), mix(one, mask, maskstr), mx), 0.0, 1.0);
	}

	// RRGGBBX mask
	else if (shadowMask == 12.0)
	{
		mask = vec3(0.0);
		pos.x = floor(mod(pos.x,7.0));
		if      (pos.x < 0.5)  mask   = 0.0.xxx;
		else if (pos.x < 2.5)  mask.r = 1.0;
		else if (pos.x < 4.5)  mask.g = 1.0;
		else                   mask.b = 1.0;
		mask = clamp(mix( mix(one, mask, mcut), mix(one, mask, maskstr), mx), 0.0, 1.0);
	}

	// 4k mask
	else
	{
		mask = vec3(0.0);
		pos.x = floor(mod(pos.x,6.0));
		if      (pos.x < 0.5)  mask   = 0.0.xxx;
		else if (pos.x < 1.5)  mask.r = 1.0;
		else if (pos.x < 2.5)  mask.rg = 1.0.xx;
		else if (pos.x < 3.5)  mask.rgb = 1.0.xxx;
		else if (pos.x < 4.5)  mask.gb = 1.0.xx;
		else                   mask.b = 1.0;
		mask = clamp(mix( mix(one, mask, mcut), mix(one, mask, maskstr), mx), 0.0, 1.0);
	}

	if (mask_layout > 0.5) mask = mask.rbg;
	float maskmin = min(min(mask.r,mask.g),mask.b);
	return (mask - maskmin) * (1.0 + (maskboost-1.0)*mb) + maskmin;
}


float SlotMask(vec2 pos, float m, float swidth)
{
	if ((slotmask + slotmask1) == 0.0) return 1.0;
	else
	{
	pos.y = floor(pos.y/slotms);
	float mlen = swidth*2.0;
	float px = floor(mod(pos.x, 0.99999*mlen));
	float py = floor(fract(pos.y/(2.0*double_slot))*2.0*double_slot);
	float slot_dark = mix(1.0-slotmask1, 1.0-slotmask, m);
	float slot = 1.0;
	if (py == 0.0 && px < swidth) slot = slot_dark; else
	if (py == double_slot && px >= swidth) slot = slot_dark;

	return slot;
	}
}

vec2 Warp(vec2 pos)
{
	pos  = pos*2.0-1.0;
	pos  = mix(pos, vec2(pos.x*inversesqrt(1.0-c_shape*pos.y*pos.y), pos.y*inversesqrt(1.0-c_shape*pos.x*pos.x)), vec2(warpX, warpY)/c_shape);
	return pos*0.5 + 0.5;
}

vec2 Overscan(vec2 pos, float dx, float dy){
	pos=pos*2.0-1.0;
	pos*=vec2(dx,dy);
	return pos*0.5+0.5;
}

float humbar(float pos)
{
	if (barintensity == 0.0) return 1.0;
	pos = (barintensity >= 0.0) ? pos : (1.0-pos);
	pos = fract(pos + mod(float(frame),barspeed)/(barspeed-1.0));
	pos = (barintensity <  0.0) ? pos : (1.0-pos);
	return (1.0-barintensity) + barintensity*pos;
}


float corner(vec2 pos) {
	vec2 b = vec2(bsize1, bsize1) *  vec2(1.0, target_size.x/target_size.y) * 0.05;
	pos = clamp(pos, 0.0, 1.0);
	pos = abs(2.0*(pos - 0.5));
	float csize1 = mix(400.0, 7.0,  pow(4.0*csize, 0.10));
	float crn = dot(pow(pos, csize1.xx), vec2(1.0, target_size.y/target_size.x));
	crn = (csize == 0.0) ? max(pos.x, pos.y) : pow(crn, 1.0/csize1);
	pos = max(pos, crn);
	vec2 res = (bsize1 == 0.0) ? 1.0.xx : mix(0.0.xx, 1.0.xx, smoothstep(1.0.xx, 1.0.xx-b, sqrt(pos)));
	res = pow(res, sborder.xx);
	return sqrt(res.x*res.y);
}


vec3 plant (vec3 tar, float r)
{
	float t = max(max(tar.r,tar.g),tar.b) + 0.00001;
	return tar * r / t;
}

vec3 declip(vec3 c, float b)
{
	float m = max(max(c.r,c.g),c.b);
	if (m > b) c = c*b/m;
	return c;
}

float igc(float mc)
{
	return pow(mc, gamma_c);
}

// noise function:
// Dedicated to the public domain.
// If you want a real license, you may consider this MIT/BSD/CC0/WTFPL-licensed (take your pick).
// Adapted from ChuckNorris - shadertoy: https://www.shadertoy.com/view/XtK3Dz

vec3 noise(vec3 v){
    if (addnoised < 0.0) v.z = -addnoised; else v.z = mod(v.z,6001.0)/1753.0;
	// ensure reasonable range
    v = fract(v) + fract(v*1e4) + fract(v*1e-4);
    // seed
    v += vec3(0.12345, 0.6789, 0.314159);
    // more iterations => more random
    v = fract(v*dot(v, v)*123.456);
    v = fract(v*dot(v, v)*123.456);
	v = fract(v*dot(v, v)*123.456);
	v = fract(v*dot(v, v)*123.456);
    return v;
}

void fetch_pixel (inout vec3 c, inout vec3 b, inout vec3 g, vec2 coord, vec2 bcoord)
{
		float stepx = 1.0 / target_size.x;
		float stepy = 1.0 / target_size.y;

		float ds = decons;

		vec2 dx = vec2(stepx, 0.0);
		vec2 dy = vec2(0.0, stepy);

		float posx = 2.0*coord.x - 1.0;
		float posy = 2.0*coord.y - 1.0;

		if (dctypex > 0.025)
		{
			posx = sign(posx)*pow(abs(posx), 1.05-dctypex);
			dx = posx * dx;
		}

		if (dctypey > 0.025)
		{

			posy = sign(posy)*pow(abs(posy), 1.05-dctypey);
			dy = posy * dy;
		}

		vec2 rc = deconrr * dx + deconrry*dy;
		vec2 gc = deconrg * dx + deconrgy*dy;
		vec2 bc = deconrb * dx + deconrby*dy;

		float r1 = NTSCA2_tex(coord + rc).r;
		float g1 = NTSCA2_tex(coord + gc).g;
		float b1 = NTSCA2_tex(coord + bc).b;

		vec3 d = vec3(r1, g1, b1);
		c = clamp(mix(c, d, ds), 0.0, 1.0);

		r1 = VBLOOM_tex(bcoord + rc).r;
		g1 = VBLOOM_tex(bcoord + gc).g;
		b1 = VBLOOM_tex(bcoord + bc).b;

		d = vec3(r1, g1, b1);
		b = g = mix(b, d, min(ds,1.0));

		r1 = VBLUR_tex(bcoord + rc).r;
		g1 = VBLUR_tex(bcoord + gc).g;
		b1 = VBLUR_tex(bcoord + bc).b;

		d = vec3(r1, g1, b1);
		g = mix(g, d, min(ds,1.0));
}


vec4 hook() {
	vec4 SourceSize = vec4(MAIN_size, 1.0 / MAIN_size.x, 1.0 / MAIN_size.y);

	float lum = AVGLUM_tex(vec2(0.5,0.5)).a;

	float gamma_in = 1.0/INTERLACE_tex(vec2(0.25,0.25)).a;
	float intera = INTERLACE_tex(vec2(0.75,0.25)).a;
	bool interb  = (intera < 0.5 || no_scanlines > 0.025);

	// Calculating texel coordinates

	vec2 texcoord = NTSCA2_pos;
	if (IOS > 0.0 && !interb){
		vec2 ofactor = target_size / MAIN_size;
		vec2 intfactor = (IOS < 2.5) ? floor(ofactor) : ceil(ofactor);
		vec2 diff = ofactor/intfactor;
		float scan = diff.y;
		texcoord = Overscan(texcoord, scan, scan);
		if (IOS == 1.0 || IOS == 3.0) texcoord = vec2(NTSCA2_pos.x, texcoord.y);
	}

	float factor  = 1.00 + (1.0-0.5*OS)*BLOOM/100.0 - lum*BLOOM/100.0;
	texcoord  = Overscan(texcoord, factor, factor);

	texcoord = Overscan(texcoord, (MAIN_size.x - overscanX)/MAIN_size.x, (MAIN_size.y - overscanY)/MAIN_size.y);

	vec2 pos1 = NTSCA2_pos.xy;
	vec2 pos  = Warp(texcoord);
	vec2 pos0 = Warp(NTSCA2_pos.xy);

	// color and bloom fetching
	vec3 color = NTSCA2_tex(pos1).rgb;
	vec3  Bloom = VBLOOM_tex(pos).rgb;
	vec3  Glow = VBLUR_tex(pos).rgb;

if ((abs(deconrr) + abs(deconrry) + abs(deconrg) + abs(deconrgy) + abs(deconrb) + abs(deconrby)) > 0.2)
	fetch_pixel(color, Bloom, Glow, pos1, pos); // deconvergence

	float cm = igc(max(max(color.r,color.g),color.b));
	float mx1 = NTSCA2_tex(pos1     ).a;
	float colmx = max(mx1, cm);
	float w3 = min((cm + 0.0001) / (colmx + 0.0005), 1.0); if(interb) w3 = 1.0;

	vec2 dx = vec2(0.001, 0.0);
	float mx0 = NTSCA2_tex(pos1 - dx).a;
	float mx2 = NTSCA2_tex(pos1 + dx).a;
	float mxg = max(max(mx0,mx1),max(mx2,cm));
	float mx = pow(mxg, 1.40/gamma_in);

	// mask boost tweak

	dx = vec2(1.0 / MAIN_size.x, 0.0)*0.25;
	mx0 = NTSCA2_tex(pos1 - dx).a;
	mx2 = NTSCA2_tex(pos1 + dx).a;
	float mb = (1.0 - min(abs(mx0-mx2)/(0.5+mx1), 1.0));

	vec3 one = vec3(1.0);

	// Apply Mask

	vec3 orig1 = color;
	vec3 cmask = one;
	vec3 cmask1 = one;
	vec3 cmask2 = one;

	// mask widths and mask dark compensate (fractional part) values
	float mwidths[14] = float[14] (2.0, 3.0, 3.0, 3.0, 6.0, 2.4, 3.5, 2.4, 3.25, 3.5, 4.5, 4.25, 7.5, 6.25);

	float mwidth = mwidths[int(shadowMask)];
	float mask_compensate = fract(mwidth);

if (shadowMask > -0.5)
{
	vec2 maskcoord = gl_FragCoord.xy * 1.00001;
	vec2 scoord = maskcoord;

	mwidth = floor(mwidth) * masksize;
	float swidth = mwidth;
	bool zoomed = (abs(mask_zoom) > 0.75);
	float mscale = 1.0;
	vec2 maskcoord0 = maskcoord;
	maskcoord.y = floor(maskcoord.y/masksize);
	float mwidth1 = max(mwidth + mask_zoom, 2.0);

if ( mshift > 0.25 )
{
	float stagg_lvl = 1.0; if (fract(mshift) > 0.25) stagg_lvl = 2.0;
	float next_line = float(floor(mod(maskcoord.y, 2.0*stagg_lvl)) < stagg_lvl);
	maskcoord0.x = maskcoord0.x + next_line * 0.5 * mwidth1;
}
	maskcoord = maskcoord0/masksize; if (mask_zoom >= 0.0) maskcoord = floor(maskcoord);

if ( !zoomed )
	cmask*= Mask(floor(maskcoord), mx, mb);
else{
	mscale  = mwidth1/mwidth;
	float mlerp = fract(maskcoord.x/mscale);
	float mcoord = floor(maskcoord.x/mscale); if (shadowMask == 12.0 && mask_zoom == -2.0) mcoord = ceil(maskcoord.x/mscale);
	cmask*=mix(Mask(vec2(mcoord,maskcoord.y), mx, mb), Mask(vec2(mcoord + 1.0, maskcoord.y), mx, mb), mlerp);
}

	if (slotwidth > 0.5) swidth = slotwidth; float smask = 1.0;

	float sm_offset = 0.0; bool bsm_offset = (shadowMask == 0.0 || shadowMask == 2.0 || shadowMask == 5.0 || shadowMask == 6.0 || shadowMask == 8.0 || shadowMask == 11.0);
	if( zoomed ) { if (mask_layout < 0.5 && bsm_offset) sm_offset = 1.0; else if (bsm_offset) sm_offset = -1.0; }

	swidth = round(swidth*mscale);
	smask = SlotMask(scoord + vec2(sm_offset,0.0), mx, swidth);

	smask = clamp(smask + mix(smask_mit, 0.0, min(w3, pow(w3*max(max(orig1.r,orig1.g),orig1.b), 0.33333))), 0.0, 1.0);

	cmask2 = cmask;
	cmask*=smask;
	cmask1 = cmask;

	if (abs(mask_bloom) > 0.025)
	{
		float maxbl = max(max(max(Bloom.r,Bloom.g),Bloom.b), mxg);
		maxbl = maxbl * max(mix(1.0, 2.0-colmx, bloom_dist), 0.0);
		if (mask_bloom > 0.025) cmask = max(min(cmask + maxbl*mask_bloom, 1.0), cmask); else cmask = max(mix(cmask, cmask*(1.0-0.5*maxbl) + plant(pow(Bloom,0.35.xxx),maxbl), -mask_bloom),cmask);
	}

	color = pow(color, vec3(mask_gamma/gamma_in));
	color = color*cmask;
	color = min(color,1.0);
	color = pow(color, vec3(gamma_in/mask_gamma));

	cmask1 = min(cmask1, 1.0);
}

	float dark_compensate  = mix(max( clamp( mix (mcut, maskstr, mx),0.0, 1.0) - 1.0 + mask_compensate, 0.0) + 1.0, 1.0, mx); if(shadowMask < -0.5) dark_compensate = 1.0;
	float bb = mix(brightboost, brightboost1, mx) * dark_compensate;
	color*=bb;

	vec3  Ref = INTERLACE_tex(pos).rgb;
	float maxb = VBLOOM_tex(pos).a;
	float vig  = AFTERGLOW_tex(clamp(pos, 0.0+0.5/MAIN_size, 1.0-0.5/MAIN_size)).a;

	vec3 Bloom1 = Bloom;
	vec3 bcmask = mix(one, cmask, bmask1);
	vec3 hcmask = mix(one, cmask, hmask1);

	if (abs(bloom) > 0.025)
	{
		if (bloom < -0.01) Bloom1 = plant(Bloom, maxb);
		Bloom1 = min(Bloom1*(orig1+color), max(0.5*(colmx + orig1 - color),0.001*Bloom1));
		Bloom1 = 0.5*(Bloom1 + mix(Bloom1, mix(colmx*orig1, Bloom1, 0.5), 1.0-color));
		Bloom1 = bcmask*Bloom1 * max(mix(1.0, 2.0-colmx, bloom_dist), 0.0);
		color = pow(pow(color, vec3(mask_gamma/gamma_in)) + abs(bloom) * pow(Bloom1, vec3(mask_gamma/gamma_in)), vec3(gamma_in/mask_gamma));
	}

	if (!interb) color = declip(min(color,1.0), mix(1.0, w3, 0.6));

	if (halation > 0.01) {
		Bloom = mix(0.5*(Bloom + Bloom*Bloom), 0.75*Bloom*Bloom, colmx);
		color = color + 2.0*max((2.0*mix(maxb*maxb, maxb, colmx)-0.5*max(max(Ref.r,Ref.g),Ref.b)),0.25)*mix(1.0,w3,0.5*colmx)*hcmask*Bloom*halation; }
	else
	if (halation < -0.01) {
		float mbl = max(max(Bloom.r,Bloom.g),Bloom.b);
		Bloom = plant(Bloom + Ref + orig1 + Bloom*Bloom*Bloom, min(mbl*mbl,0.75));
		color = color + 2.0*mix(1.0,w3,0.5*colmx)*hcmask*Bloom*(-halation); }


	float w = 0.25 + 0.60*mix(w3, 1.0, sqrt(colmx));
	if (smoothmask > 0.5) { color = min(color,1.0); color = max(min(color/w3, 1.0)*w3, min(orig1*bb,color*(1.0-w3))); }

	if (m_glow < 0.5) Glow = mix(Glow, 0.25*color, colmx);
	else
	{
		maxb = max(max(Glow.r,Glow.g),Glow.b);
		vec3 orig2 = plant(orig1 + 0.001*Ref, 1.0);
		Bloom = plant(Glow, 1.0);
		Ref = abs(orig2-Bloom);
		mx0 = max(max(orig2.g,orig2.g),orig2.b)-min(min(orig2.g,orig2.g),orig2.b);
		mx2 = max(max(Bloom.g,Bloom.g),Bloom.b)-min(min(Bloom.g,Bloom.g),Bloom.b);
		Bloom = mix(maxb*min(Bloom,orig2), w*mix(mix(Glow, max(max(Ref.g,Ref.g),Ref.b)*Glow, max(mx,mx0)), mix(color, Glow, mx2), max(mx0,mx2)*Ref), min(sqrt((1.10-mx0)*(0.10+mx2)),1.0));
		Glow = mix(m_glow_low*Glow, m_glow_high*Bloom, pow(colmx, m_glow_dist/gamma_in));
	}

	if (m_glow < 0.5) {
		if (glow >= 0.0) color = color + 0.5*Glow*glow; else color = color + abs(glow)*min(cmask2*cmask2,1.0)*Glow; }
	else { cmask1 = clamp(mix(one, cmask1, m_glow_mask),0.0, 1.0); color = color + abs(glow)*cmask1*Glow; }

	color = min(color, 1.0);

	color = pow(color, vec3(1.0/gamma_out));

	float rc = 0.6*sqrt(max(max(color.r, color.g), color.b))+0.4;

	if (abs(addnoised) > 0.01)
	{
		vec3 noise0 = noise(vec3(floor(target_size * NTSCA2_pos / noiseresd), float(frame)));
		if (noisetype < 0.5) color = mix(color, noise0, 0.25*abs(addnoised) * rc);
		else color = min(color * mix(1.0, 1.5*noise0.x, 0.5*abs(addnoised)), 1.0);
	}

	colmx = max(max(orig1.r,orig1.g),orig1.b);
	color = color + bmask*mix(cmask2, 0.125*(1.0-colmx)*color, min(20.0*colmx, 1.0));

	return delinearize(vec4(color*vig*humbar(mix(pos.y, pos.x, bardir))*post_br*corner(pos0), 1.0));
}
