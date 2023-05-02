from moviepy.editor import VideoFileClip, concatenate_videoclips

def convert_video_to_gif(input_file, output_file, start_time=0, end_time=None, fps=10, resize_ratio=None, width=None, height=None):
    # Load the video file
    clip = VideoFileClip(input_file)

    # Trim the video if start_time and/or end_time is specified
    if start_time or end_time:
        clip = clip.subclip(start_time, end_time)

    # Resize the video if a resize_ratio is specified
    if resize_ratio:
        clip = clip.resize(resize_ratio)
    # Resize the video if width and/or height are specified
    elif width or height:
        clip = clip.resize(newsize=(width, height))

    # Set the desired FPS for the output GIF
    clip = clip.set_fps(fps)

    # Write the output GIF
    clip.write_gif(output_file)

# Example usage:
input_file = "C:/Users/Sebbi Kankondi/Documents/National Marine Aquarium/Presentations/images/hab.mp4"
output_file = "C:/Users/Sebbi Kankondi/Documents/National Marine Aquarium/Presentations/images/hab.gif"
convert_video_to_gif(input_file, output_file, start_time=7,
                      end_time=14, fps=10, resize_ratio = 1)

# convert_video_to_gif(input_file, output_file, start_time=0,
#                       end_time=4, fps=10, width=500, height=600)
