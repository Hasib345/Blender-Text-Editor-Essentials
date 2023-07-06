# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####


# Code Used by:
# Mackraken, tintwotin, Hydrocallis ,chichige-bobo#

bl_info = {
    "name": "Text Editor Essentials",
    "location": "Text Editor",
    "version": (1, 2, 0),
    "blender": (3, 0, 0),
    "description": "Better Text Editor for coding",
    "author": "Hasib345",
    "category": "Text Editor",
    "wiki_url":"https://github.com/Hasib345/Blender-Text-Editor-Essentials"
}


from .consol import TEXT_OT_run_in_console, update_assume_print
from .code_editor import (CE_OT_cursor_set, CE_OT_mouse_move, CE_OT_scroll, CE_PG_settings,
    CE_PT_settings_panel, WM_OT_mouse_catcher, 
    set_draw, update_prefs ,ce_manager
    )

import bpy
from .highlight import update_highlight , update_colors , redraw





class BT_Preference(bpy.types.AddonPreferences):
    bl_idname = __name__

    colors = {
        "BLUE": ((.25, .33, .45, 1),
                 (1, 1, 1, 1),
                 (.18, .44, .61, 1),
                 (0.14, .6, 1, .55)),

        "YELLOW": ((.39, .38, .07, 1),
                   (1, 1, 1, 1),
                   (.46, .46, 0, 1),
                   (1, .79, .09, .4)),

        "GREEN": ((.24, .39, .26, 1),
                  (1, 1, 1, 1),
                  (.2, .5, .19, 1),
                  (.04, 1., .008, .4)),

        "RED": ((.58, .21, .21, 1),
                (1, 1, 1, 1),
                (.64, .27, .27, 1),
                (1, 0.21, .21, 0.5))
    }

    menu: bpy.props.EnumProperty(
        default="Editor",
        name="Preferences Menu",
        items=(
            ("Editor", "Editor", "Editor window settings"),
            ("Highlights", "Highlight", "highlight setting tab")))

    from bpy.props import (BoolProperty, FloatVectorProperty, EnumProperty,
                           IntProperty, FloatProperty)

    enable: BoolProperty(description="Enable highlighting",
                         name="Highlight Occurrences",
                         default=True,
                         update=update_highlight)

    line_thickness: IntProperty(description="Line Thickness",
                                default=1,
                                name="Line Thickness",
                                min=1,
                                max=4)

    show_in_scroll: BoolProperty(description="Show in scrollbar",
                                 name="Show in Scrollbar",
                                 default=True)

    min_str_len: IntProperty(description="Don't search below this",
                             name='Minimum Search Length',
                             default=2,
                             min=1,
                             max=4)

    case_sensitive: BoolProperty(description='Case Sensitive Matching',
                                 name='Case Sensitive',
                                 default=False)

    col_bg: FloatVectorProperty(description='Background color',
                                name='Background',
                                default=colors['BLUE'][0],
                                subtype='COLOR',
                                size=4,
                                min=0,
                                max=1)

    col_line: FloatVectorProperty(description='Line and frame color',
                                  name='Line / Frame',
                                  default=colors['BLUE'][2],
                                  subtype='COLOR',
                                  size=4,
                                  min=0,
                                  max=1)

    fg_col: FloatVectorProperty(description='Foreground color',
                                name='Foreground',
                                default=colors['BLUE'][1],
                                size=4,
                                min=0,
                                subtype='COLOR',
                                max=1)

    col_scroll: FloatVectorProperty(description="Scroll highlight opacity",
                                    name="Scrollbar",
                                    default=colors['BLUE'][3],
                                    size=4,
                                    min=0,
                                    max=1,
                                    subtype='COLOR')

    draw_type: EnumProperty(description="Draw type for highlights",
                            name="Draw Type",
                            default="SOLID_FRAME",
                            items=(("SOLID", "Solid", "", 1),
                                   ("LINE", "Line", "", 2),
                                   ("FRAME", "Frame", "", 3),
                                   ("SOLID_FRAME", "Solid + Frame", "", 4)))

    col_preset: EnumProperty(description="Highlight color presets",
                             default="BLUE", name="Presets",
                             update=update_colors,
                             items=(("BLUE", "Blue", "", 1),
                                    ("YELLOW", "Yellow", "", 2),
                                    ("GREEN", "Green", "", 3),
                                    ("RED", "Red", "", 4),
                                    ("CUSTOM", "Custom", "", 5)))
    
    opacity: FloatProperty(
        name="Background", min=0.0, max=1.0, default=0.2, update=update_prefs
    )
    ws_alpha: FloatProperty(
        name="Whitespace Alpha", min=0.0, max=1.0, default=0.2,
        update=update_prefs
    )
    auto_width: BoolProperty(
        name="Auto Width", description="Automatically scale minimap width "
        "based on region width", default=1, update=update_prefs
    )
    minimap_width: IntProperty(
        name="Minimap Width", description="Minimap base width in px",
        min=0, max=400, default=225, update=update_prefs
    )
    window_min_width: IntProperty(
        name="Fade Threshold", description="Region width (px) threshold for "
        "fading out panel", min=0, max=4096, default=250, update=update_prefs
    )
    character_width: FloatProperty(
        name="Character Width", description="Minimap character "
        "width in px", min=0.1, max=4.0, default=1.0, update=update_prefs
    )
    line_height: FloatProperty(
        name="Line Height", description="Minimap line height in "
        "pixels", min=0.5, max=4.0, default=1.0, update=update_prefs
    )
    indent_trans: FloatProperty(
        name="Indent Guides", description="0 - fully opaque, 1 - fully "
        "transparent", min=0.0, max=1.0, default=0.3, update=update_prefs
    )
    large_tabs: BoolProperty(
        name="Bigger Tabs", description="Increase tab size for bigger "
        "monitors", update=update_prefs
    )
    tabs_right: BoolProperty(
        name="Tabs Right Side", description="Place text tabs to the right of"
        "minimap", update=update_prefs
    )
    assume_print: bpy.props.BoolProperty(
        name="Assume Print (WARNING: Unstable)",
        description="Hijack prints from other scripts and display them in "
        "Blender's\nconsole. Experimental and may crash. Use at own risk",
        default=False,
        update=update_assume_print
    )

    persistent: BoolProperty(name="Persistent", description=""
                             "Access bindings in console after execution")

    clear_bindings: BoolProperty(name="Clear Bindings", description="Clear "
                                 "console bindings before running text block")

    keep_math: BoolProperty(default=True, name="Keep Math", description=""
                            "Restore math on clear")

    keep_mathutils: BoolProperty(description="Restore mathutils on clear",
                                 default=True, name="Keep Mathutils")

    keep_vars: BoolProperty(description="Restore convenience variables",
                            default=True, name="Keep C, D variables")

    show_name: BoolProperty(default=True, name="Show Name",
                            description="Display text name in console")

    show_time: BoolProperty(description="Display elapsed time after execution",
                            default=True, name="Show Elapsed", )


    del (BoolProperty, FloatVectorProperty,
         EnumProperty, IntProperty, FloatProperty)

    def draw(self, context):

        layout = self.layout
        row = layout.row()
        row.alignment = 'CENTER'
        row.scale_x = 0.5
        row.scale_y = 1.4
        row.prop(self, "menu", expand=True)
        layout.separator(factor=2)

        mainrow = layout.row()
        mainrow.alignment = 'CENTER'


        if self.menu == 'Highlights':
            row = layout.row()
            col = row.column()
            col.prop(self, 'show_in_scroll')
            col.prop(self, 'case_sensitive')
            col = row.column()
            col.prop(self, 'min_str_len')
            col.prop(self, "line_thickness")

            split = layout.split()
            col = split.column()

            split.column()
            
            col.enabled = self.draw_type in {'LINE', 'FRAME', 'SOLID_FRAME'}
            layout.row().prop(self, "draw_type", expand=True)
            layout.grid_flow(align=True).prop(self, "col_preset", expand=True)

            if self.col_preset == 'CUSTOM':
                split = layout.column().split()
                for item in ["col_bg", "fg_col", "col_line", "col_scroll"]:
                    split.column().prop(self, item)
        elif self.menu == 'Editor':
            layout = self.layout
            layout.use_property_split = True
            layout.scale_y = 1.0
            row = layout.row()
            col = row.column()

            flow = col.grid_flow(columns=2, even_columns=1)
            flow.prop(self, "large_tabs")
            flow.prop(self, "auto_width")
            row = flow.row()
            if self.auto_width:
                row.active = False
            row.prop(self, "minimap_width", slider=True )
            flow = col.grid_flow(columns=2, even_columns=1)
            flow.prop(self, "opacity", slider=True)
            flow.prop(self, "character_width")
            flow.prop(self, "line_height")

            flow.prop(self, "ws_alpha", slider=True)
            flow.prop(self, "indent_trans", slider=True)
            flow.prop(self, "window_min_width")
            row.separator()

            layout.separator()


def text_editor_add(self, context):
    layout = self.layout
    row = layout.row(align = True)
    row.operator("preferences.addon_show", text="", icon = 'SETTINGS').module = __name__
    row.popover(CE_PT_settings_panel.bl_idname)
    row = layout.row(align=True)
    text = "" if "context_menu" not in self.bl_idname else "Run In Console"
    row.operator("text.run_in_console", text=text, icon='CONSOLE')
    if not TEXT_OT_run_in_console.any_console(context):
        row.enabled = False
    row.popover('TEXT_PT_run_in_console_settings')

from bpy.types import Screen, TEXT_HT_header

classes = (
    WM_OT_mouse_catcher,
    CE_OT_mouse_move,
    CE_OT_cursor_set,
    CE_OT_scroll,
    CE_PT_settings_panel,
    CE_PG_settings,
    BT_Preference,
    )
from . import intellisense, search_online, templetes, expand , consol , highlight

modules = [intellisense, search_online, templetes, expand , consol , highlight]

def register():

    for cls in classes:
        bpy.utils.register_class(cls)
    
    TEXT_HT_header.append(text_editor_add)
    
    Screen.code_editors = bpy.props.CollectionProperty(type=CE_PG_settings)
    kc = bpy.context.window_manager.keyconfigs.addon.keymaps
    km = kc.get('Text', kc.new('Text', space_type='TEXT_EDITOR'))
    new = km.keymap_items.new
    kmi1 = new('ce.mouse_move', 'MOUSEMOVE', 'ANY', head=True)
    kmi2 = new('ce.cursor_set', 'LEFTMOUSE', 'PRESS', head=True)
    register.keymaps = ((km, kmi1), (km, kmi2))
    set_draw(getattr(bpy, "context"))
    import addon_utils
    mod = addon_utils.addons_fake_modules.get(__name__)
    if mod:
        addon_utils.module_bl_info(mod)["show_expanded"] = True

    
    # Highlight
    

    
    for module in modules:
        module.register()


def unregister():


    for module in modules:
        module.unregister()

    TEXT_HT_header.remove(text_editor_add)
    set_draw(state=False)
    for km, kmi in register.keymaps:
        km.keymap_items.remove(kmi)
    del register.keymaps
    ce_manager.nuke()
    for w in bpy.context.window_manager.windows:
        w.screen.code_editors.clear()
    del bpy.types.Screen.code_editors


    for cls in classes:
        bpy.utils.unregister_class(cls)
    redraw(getattr(bpy, "context"))


    