# Retro console shaders for MPV

Port of some [libretro] shaders for use with [mpv].
Useful for watching Tool Assisted Speedruns at native resolution from
<https://tasvideos.org/>.

## Usage

1. Clone this repository inside mpv's config folder:

   ```
   mkdir -p ~/.config/mpv/shaders
   git clone https://git.sr.ht/~taiite/mpv-retro-shaders ~/.config/mpv/shaders/mpv-retro-shaders
   ```

2. Add this snippet at the top of your `mpv.conf` configuration file:

   ```
   include=~~/shaders/mpv-retro-shaders/all.conf
   ```

3. Start mpv with a shader profile, e.g. `crt-lottes`:

   ```
   mpv --profile=crt-lottes my_video.mp4
   ```

## Available shaders

- CRT guest advanced shader (`--profile=crt-guest-advanced-ntsc`) emulates CRT
  looks. See [crtgan] for info on parameters.

- Lottes' CRT shader (`--profile=crt-lottes`) emulates several CRT looks.
  Change the `SHADOW_MASK` parameter (0-4) to switch CRT types.
  Change the `CURVATURE` parameter to set the curvature of the screen.

- Gameboy Advance color shader (`--profile=gba`) make the colors look as if on a
  GBA screen.

## License

Each shader is distributed under a different license.
See their source for details.

[crtgan]: https://github.com/libretro/slang-shaders/blob/8595c3cbea2120bc9b82e4ff756f61100543ec83/crt/shaders/guest/advanced/README_old
[libretro]: https://github.com/libretro/glsl-shaders
[mpv]: https://mpv.io
