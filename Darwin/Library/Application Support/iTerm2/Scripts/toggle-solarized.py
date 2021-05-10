#!/usr/bin/env python3.7

# Run:
#     /Applications/iTerm.app/Contents/Resources/it2run ~/Library/Application\ Support/iTerm2/Scripts/toggle-solarized.py
#
# See also https://iterm2.com/python-api/tutorial/running.html

import iterm2

# Profiles to update
g_profiles = ['Default']


def colors_equal(c1, c2):
    return (
        round(c1.red) == round(c2.red) and
        round(c1.green) == round(c2.green) and
        round(c1.blue) == round(c2.blue) and
        round(c1.alpha) == round(c2.alpha) and
        c1.color_space == c2.color_space
    )


async def current_preset_is(connection, preset_name):
    app = await iterm2.async_get_app(connection)
    session = app.current_terminal_window.current_tab.current_session
    current_profile = await session.async_get_profile()

    preset = await iterm2.ColorPreset.async_get(connection, preset_name)
    return all(colors_equal(current_profile.get_color_with_key(color.key),
                            color)
               for color in preset.values)


async def main(connection):
    dark_p = await current_preset_is(connection, 'Solarized Dark')
    preset_name = 'Solarized Light' if dark_p else 'Solarized Dark'
    preset = await iterm2.ColorPreset.async_get(connection, preset_name)

    profiles = await iterm2.PartialProfile.async_query(connection)
    for partial in profiles:
        if partial.name in g_profiles:
            await partial.async_set_color_preset(preset)


iterm2.run_until_complete(main)
