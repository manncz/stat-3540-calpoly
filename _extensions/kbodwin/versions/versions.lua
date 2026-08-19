-- versions.lua
-- Source-preserving Quarto/Pandoc Lua filter.
--
-- This filter writes one `.qmd` file per requested version.  Unlike a normal
-- Pandoc AST round-trip, it reads the original `.qmd` source file as text and
-- copies the original lines forward, removing only source blocks that do not
-- belong in the target version.
--
-- Why this file exists:
--   pandoc.write(doc, "markdown") serializes the Pandoc AST back to markdown.
--   That is convenient, but it is not source-preserving: Quarto code cells can
--   appear as `::: cell` wrappers, YAML can include Quarto-computed metadata,
--   comments are lost, and formatting can change.  This filter avoids that by
--   using the AST only for the `versions:` configuration, while using the raw
--   source text for generated `.qmd` files.
--
-- Supported version markers:
--
--   ::: {.version-A}
--   Appears in version A.
--   :::
--
--   ::: {.version-A-XX}
--   Appears in subversion A/XX.
--   :::
--
--   ::: {.version-A .version-B}
--   Appears in A and B.
--   :::
--
--   ```{r version="A"}
--   1 + 1
--   ```
--
--   ```{r version="A B"}
--   1 + 1
--   ```
--
--   ```{r}
--   #| version: [A, B]
--   1 + 1
--   ```
--
-- Supported YAML form:
--
--   versions:
--     - A
--     - B:
--         out-dir: "../student/B"
--         out-file: "exam_1_version_B.qmd"
--         render: true
--         yaml:
--           title: "Exam 1 -- Version B"
--     - none:
--         ignore: true
--
-- In this list-style schema, an item with `ignore: true` is not generated as
-- an output version.  Instead, its name becomes the global ignore tag.  For
-- example, `.version-none` and `version: none` are removed from every output.
--
-- The generated subfiles always omit top-level `versions`, because that key is
-- configuration for this splitter.  The generated subfiles keep ordinary YAML,
-- including `params`, unless a per-version YAML override replaces that key.
-- For `filters`, the generated subfiles keep filters other than this splitter
-- filter.  Removing only this splitter prevents recursive splitting while still
-- allowing ordinary filters to run when generated files are rendered.
--
-- Default output paths:
--   A/<source-filename>.qmd
--   A/XX/<source-filename>.qmd
--
-- If out-dir is supplied, it is treated as an output root and resolved
-- relative to the current working directory.  The version folder is still
-- created underneath that root unless out-dir already ends with the version
-- name.  Subversion folders are created underneath the main version folder.
-- If out-file is supplied, it must be only a filename, not a path.

-- Load Pandoc's system module for file-system operations such as getting the
-- working directory and creating directories.
local system = require 'pandoc.system'

-- Load Pandoc's path module for path joining, normalization, and extracting a
-- filename from a full source path.
local path = require 'pandoc.path'

-- Fallback filename used only if neither Quarto nor Pandoc tells us the input
-- filename.  In normal Quarto renders, this fallback should not be used.
local FALLBACK_SOURCE_FILE = 'versioned.qmd'

-- Internal patch marker so you can verify you are using the corrected file.
local SPLIT_VERSIONS_SOURCE_PATCH = '2026-06-19-list-schema-output-paths-fixed'

-- When true, version markers are removed from kept source blocks.  For example:
--   ::: {.version-A .callout-note}
-- becomes:
--   ::: {.callout-note}
-- If the only attribute on a Div is the version marker, the Div fence itself is
-- removed and only the inner content remains.
local STRIP_VERSION_MARKERS = true

-- When false, A/source.qmd gets untagged content plus `.version-A` content, but
-- not `.version-A-XX` content.  A/XX/source.qmd gets untagged content,
-- `.version-A` content, and `.version-A-XX` content.
local INCLUDE_SUBVERSIONS_IN_MAIN = false

-- After versioned blocks are removed, source files can be left with very large
-- vertical gaps where a removed block used to be.  This keeps the generated
-- files readable while still preserving ordinary paragraph spacing.  Set this
-- to nil if you want to preserve every blank line exactly.
local MAX_CONSECUTIVE_BLANK_LINES = 1

-- Top-level YAML keys that should never be copied into generated subfiles.
--
-- * versions: this is configuration for the splitter only.
--
-- Do not put `filters` or `params` here:
--   * `filters` is cleaned separately so that only this splitter filter is
--     removed while all other filters are preserved.
--   * `params` should be preserved when the original document or a per-version
--     YAML override explicitly supplies it; we simply avoid inventing it.
local OMIT_GENERATED_YAML_KEYS = {
  versions = true
}

-- Main version specs keyed by version name.  Example:
--   version_by_name["A"].out_dir
--   version_by_name["A"].yaml
local version_by_name = {}

-- Main versions in user-specified order.  Lua table iteration order is not
-- guaranteed, so this list gives stable output order.
local version_order = {}

-- Main versions sorted longest-first for tag parsing.  This avoids misreading
-- a main version named `A-B` as main `A` plus subversion `B`.
local versions_for_matching = {}

-- Optional ignore label.  If the YAML list contains:
--
--   - none:
--       ignore: true
--
-- then version marker `none` is excluded from every generated file.
local ignore_label = nil

-- Shared defaults for all versions.  This remains as an internal structure for
-- merging, but the supported user-facing schema is now the list-style
-- `versions:` sequence above, not the older `{ignore, items}` map schema.
local defaults = {
  out_dir = nil,
  out_file = nil,
  yaml = nil,
  render = nil
}

-- Keys that are configuration keys rather than version names when parsing one
-- list item such as `- name: A` or `- none: {ignore: true}`.
local RESERVED_VERSION_KEYS = {
  ['ignore'] = true,
  ['items'] = true,
  ['list'] = true,
  ['versions'] = true,
  ['defaults'] = true,
  ['out-dir'] = true,
  ['out_dir'] = true,
  ['out-file'] = true,
  ['out_file'] = true,
  ['yaml'] = true,
  ['metadata'] = true,
  ['render'] = true
}

-- Print a diagnostic message to stderr.  This keeps status messages out of the
-- rendered document.
local function log(message)
  io.stderr:write('[versions] ' .. tostring(message) .. '\n')
end

-- Remove leading and trailing whitespace from a string.
local function trim(s)
  s = tostring(s or '')
  return (s:gsub('^%s+', ''):gsub('%s+$', ''))
end

-- Return true when string `s` starts with string `prefix`.
local function starts_with(s, prefix)
  return s:sub(1, #prefix) == prefix
end

-- Return a plain string for a Pandoc metadata value.
local function meta_to_string(x)
  if x == nil then
    return nil
  end

  -- YAML booleans such as `render: true` usually arrive as Lua booleans.
  -- Convert them explicitly before falling back to Pandoc's stringify helper.
  if type(x) == 'boolean' then
    return x and 'true' or 'false'
  end

  return trim(pandoc.utils.stringify(x))
end

-- Convert a Pandoc/Lua metadata value to a Boolean when possible.
--
-- YAML `render: true` normally arrives as a Boolean.  This helper also accepts
-- string-like values such as `yes`, `no`, `1`, and `0` because those can appear
-- after metadata has been stringified or edited by other tooling.
local function meta_to_boolean(x)
  if x == nil then
    return nil
  end

  if type(x) == 'boolean' then
    return x
  end

  local s = string.lower(meta_to_string(x) or '')

  if s == 'true' or s == 'yes' or s == 'y' or s == '1' then
    return true
  end

  if s == 'false' or s == 'no' or s == 'n' or s == '0' then
    return false
  end

  return nil
end

-- Return true if `x` is a Pandoc List.  YAML sequences usually arrive in Lua
-- filters as Pandoc Lists.
local function is_list(x)
  return pandoc.utils.type(x) == 'List'
end

-- Return true if `x` behaves like a Lua/Pandoc metadata map.
local function is_map(x)
  return type(x) == 'table' and pandoc.utils.type(x) == 'table'
end

-- Fetch a value from a map using any of several allowed spellings.  This lets
-- the YAML use either `out-dir` or `out_dir`, for example.
local function map_get(map, keys)
  if not is_map(map) then
    return nil
  end

  for _, key in ipairs(keys) do
    if map[key] ~= nil then
      return map[key]
    end
  end

  return nil
end

-- Make a recursive copy of a table, preserving metatables.  Preserving
-- metatables matters for Pandoc values such as MetaMaps and MetaLists.
local function deep_copy(x, seen)
  if type(x) ~= 'table' then
    return x
  end

  seen = seen or {}

  if seen[x] then
    return seen[x]
  end

  local out = {}
  seen[x] = out

  for k, v in pairs(x) do
    out[deep_copy(k, seen)] = deep_copy(v, seen)
  end

  return setmetatable(out, getmetatable(x))
end

-- Merge key/value pairs from `overlay` into `base`.  Nested maps are merged;
-- non-map values from `overlay` replace the corresponding value in `base`.
local function merge_map_in_place(base, overlay)
  if not is_map(overlay) then
    return base
  end

  for k, v in pairs(overlay) do
    if is_map(v) and is_map(base[k]) then
      merge_map_in_place(base[k], v)
    else
      base[k] = deep_copy(v)
    end
  end

  return base
end

-- Return a new map that is `base` with `overlay` merged into it.
local function merged_meta(base, overlay)
  local out = deep_copy(base or {})
  return merge_map_in_place(out, overlay)
end

-- Convert a YAML sequence of one-key maps into a single map.  This supports
-- this style:
--
--   - out-dir: ../student
--   - out-file: exam.qmd
--
-- as equivalent to:
--
--   out-dir: ../student
--   out-file: exam.qmd
local function list_of_maps_to_map(xs)
  local out = {}

  if not is_list(xs) then
    return out
  end

  for _, item in ipairs(xs) do
    if is_map(item) then
      for k, v in pairs(item) do
        out[k] = v
      end
    end
  end

  return out
end

-- Normalize a version option block.  Most users will write a map; this also
-- accepts a list of maps for flexibility.
local function normalize_options(opts)
  if is_map(opts) then
    return opts
  end

  if is_list(opts) then
    return list_of_maps_to_map(opts)
  end

  return nil
end

-- Sanitize automatically generated directory names such as `A` or `XX`.  This
-- is not applied to user-supplied `out-dir`; if the user supplies out-dir, we
-- assume they intentionally supplied a path.
local function safe_path_part(s)
  s = trim(s)
  s = s:gsub('[/\\]', '_')

  if s == '' or s == '.' or s == '..' then
    error('Unsafe empty/dot path component for version/subversion')
  end

  return s
end

-- Validate an output filename.  Paths belong in `out-dir`; `out-file` should be
-- only a filename.
local function safe_file_name(s)
  s = trim(s)

  if s == '' or s == '.' or s == '..' then
    error('Unsafe output filename: ' .. tostring(s))
  end

  if s:match('[/\\]') then
    error('`out-file` must be a filename, not a path: ' .. s)
  end

  return s
end

-- Create a directory if it does not already exist.  Passing `true` creates
-- parent directories as needed.
local function ensure_dir(dir)
  local ok = pcall(function()
    system.list_directory(dir)
  end)

  if not ok then
    system.make_directory(dir, true)
  end
end

-- Read an entire text file into a string.
local function read_text_file(filename)
  local f, err = io.open(filename, 'r')

  if not f then
    error('Could not open source file ' .. filename .. ': ' .. tostring(err))
  end

  local text = f:read('*a')
  f:close()
  return text
end

-- Write an entire text file, replacing the file if it already exists.
local function write_text_file(filename, text)
  local f, err = io.open(filename, 'w')

  if not f then
    error('Could not open ' .. filename .. ' for writing: ' .. tostring(err))
  end

  f:write(text)
  f:close()
end

-- Return a table's keys in sorted order for deterministic output.
local function sorted_keys(t)
  local keys = {}

  for k, _ in pairs(t or {}) do
    table.insert(keys, k)
  end

  table.sort(keys)
  return keys
end

-- Copy a list-like table.
local function copy_list(xs)
  local out = {}

  for i, x in ipairs(xs or {}) do
    out[i] = x
  end

  return out
end

-- Return the source input path.  Quarto exposes `quarto.doc.input_file`; plain
-- Pandoc exposes `PANDOC_STATE.input_files`.
local function get_source_file_path()
  local input_file = nil

  if quarto and quarto.doc and quarto.doc.input_file then
    input_file = quarto.doc.input_file
  elseif PANDOC_STATE and PANDOC_STATE.input_files and PANDOC_STATE.input_files[1] then
    input_file = PANDOC_STATE.input_files[1]
  end

  if input_file and input_file ~= '' and input_file ~= '-' then
    if path.is_absolute(input_file) then
      return path.normalize(input_file)
    end

    return path.normalize(path.join({ system.get_working_directory(), input_file }))
  end

  return nil
end

-- Return only the source filename, not the whole path.
local function get_source_file_name(source_path)
  if source_path and source_path ~= '' then
    return path.filename(source_path)
  end

  return FALLBACK_SOURCE_FILE
end

-- Resolve an output directory relative to the current working directory unless
-- the configured path is already absolute.
local function resolve_dir(dir)
  if path.is_absolute(dir) then
    return path.normalize(dir)
  end

  return path.normalize(path.join({ system.get_working_directory(), dir }))
end

-- Return the last path component in a path-like string.  Pandoc's path helpers
-- are used for normalization/joining, but this tiny helper deliberately accepts
-- both `/` and `\` separators so the duplicate-avoidance logic below works on
-- macOS, Linux, and Windows-style paths.
local function last_path_part(p)
  p = tostring(p or '')
  p = p:gsub('[/\\]+$', '')

  if p == '' then
    return ''
  end

  return p:match('([^/\\]+)$') or p
end

-- Append a path component unless the directory already ends with that exact
-- component.  This lets all of these produce the same main-version folder:
--
--   out-dir: generated
--   out-dir: generated/A
--   out-dir: A
--
-- for version `A`.  The first becomes `generated/A`; the latter two are left
-- alone instead of becoming `generated/A/A` or `A/A`.
local function append_path_part_if_missing(dir, part)
  part = safe_path_part(part)

  if last_path_part(dir) == part then
    return path.normalize(dir)
  end

  return path.normalize(path.join({ dir, part }))
end

-- Return true when two normalized paths point to the same text path.  This is a
-- defensive guard against accidentally writing a generated file over the source
-- `.qmd`.  It is string-based rather than inode-based so it also catches paths
-- before a file exists.
local function same_normalized_path(a, b)
  if not a or not b then
    return false
  end

  return path.normalize(a) == path.normalize(b)
end

-- Replace placeholders in `out-dir` or `out-file` strings.
local function apply_placeholders(s, target, source_file)
  s = tostring(s)
  s = s:gsub('{source}', source_file)
  s = s:gsub('{version}', target.main or '')
  s = s:gsub('{subversion}', target.sub or '')
  return s
end

-- Sort version names longest-first for correct parsing of labels such as
-- `A-B-XX` when both `A` and `A-B` are legal main versions.
local function sort_versions_for_matching(versions)
  local out = copy_list(versions)

  table.sort(out, function(a, b)
    if #a == #b then
      return a < b
    end

    return #a > #b
  end)

  return out
end

-- Create an empty version spec.
local function new_version_spec(name)
  return {
    name = name,
    out_dir = nil,
    out_file = nil,
    yaml = nil,
    render = nil,
    subversions = {}
  }
end

-- Get the spec for a main version, creating it if needed.
local function get_or_create_version_spec(name)
  name = trim(name)

  if name == '' then
    error('Version names may not be empty')
  end

  if not version_by_name[name] then
    version_by_name[name] = new_version_spec(name)
    table.insert(version_order, name)
  end

  return version_by_name[name]
end

-- Apply an options map to either a main version spec or a subversion spec.
local function apply_options_to_spec(spec, opts)
  opts = normalize_options(opts)

  if not opts then
    return
  end

  local out_dir = map_get(opts, { 'out-dir', 'out_dir', 'outdir' })
  if out_dir ~= nil then
    spec.out_dir = meta_to_string(out_dir)
  end

  local out_file = map_get(opts, { 'out-file', 'out_file', 'outfile' })
  if out_file ~= nil then
    spec.out_file = meta_to_string(out_file)
  end

  local yaml = map_get(opts, { 'yaml', 'metadata' })
  if yaml ~= nil then
    spec.yaml = merged_meta(spec.yaml or {}, yaml)
  end

  local render = map_get(opts, { 'render' })
  if render ~= nil then
    local render_bool = meta_to_boolean(render)

    if render_bool == nil then
      error('The `render` option for version ' .. tostring(spec.name or '<subversion>') .. ' must be true or false.')
    end

    spec.render = render_bool
  end

  -- Accept either a normal map under `subversions:` or a list of one-key maps.
  -- Normalizing before `pairs()` also keeps LuaLS from warning about `nil`.
  local subs = normalize_options(map_get(opts, { 'subversions', 'sub-versions', 'subs' }))

  -- Use a direct `type(subs) == 'table'` guard instead of the custom `is_map`
  -- helper here so Lua language servers can see that `pairs(subs)` is safe.
  -- This avoids the "Cannot assign unknown|nil to parameter table" diagnostic.
  if type(subs) == 'table' then
    for sub_name, sub_opts in pairs(subs) do
      sub_name = trim(sub_name)
      spec.subversions[sub_name] = spec.subversions[sub_name] or {
        out_dir = nil,
        out_file = nil,
        yaml = nil,
        render = nil
      }
      apply_options_to_spec(spec.subversions[sub_name], sub_opts)
    end
  end
end

-- Apply options to shared defaults rather than to one specific version.
local function apply_options_to_defaults(opts)
  opts = normalize_options(opts)

  if not opts then
    return
  end

  local out_dir = map_get(opts, { 'out-dir', 'out_dir', 'outdir' })
  if out_dir ~= nil then
    defaults.out_dir = meta_to_string(out_dir)
  end

  local out_file = map_get(opts, { 'out-file', 'out_file', 'outfile' })
  if out_file ~= nil then
    defaults.out_file = meta_to_string(out_file)
  end

  local yaml = map_get(opts, { 'yaml', 'metadata' })
  if yaml ~= nil then
    defaults.yaml = merged_meta(defaults.yaml or {}, yaml)
  end

  local render = map_get(opts, { 'render' })
  if render ~= nil then
    local render_bool = meta_to_boolean(render)

    if render_bool == nil then
      error('The default `render` option must be true or false.')
    end

    defaults.render = render_bool
  end
end

-- Return true when an option map marks a list item as the ignore tag.
--
-- The intended user-facing syntax is:
--
--   versions:
--     - A
--     - none:
--         ignore: true
--
-- In that example, `none` is not generated as an output version.  Instead,
-- `.version-none` and `version: none` are removed from every generated file.
local function opts_mark_ignore(opts)
  opts = normalize_options(opts)

  if not opts then
    return false
  end

  local ignore = map_get(opts, { 'ignore' })

  if ignore == nil then
    return false
  end

  local value = meta_to_boolean(ignore)

  if value == nil then
    error('The `ignore` option in `versions:` must be true or false.')
  end

  return value
end

-- Set the global ignore label.  More than one ignore label is ambiguous, so the
-- filter stops with a clear error instead of guessing.
local function set_ignore_label(label)
  label = trim(label)

  if label == '' then
    error('The ignore version label may not be empty.')
  end

  if ignore_label and ignore_label ~= label then
    error('Only one `ignore: true` entry is allowed in `versions:`. Found both `' .. ignore_label .. '` and `' .. label .. '`.')
  end

  ignore_label = label
end

-- Parse one item in the YAML `versions:` list.
--
-- Supported examples:
--
--   - A
--
--   - B:
--       out-dir: ../student
--       out-file: exam_B.qmd
--       render: true
--       yaml:
--         title: Exam B
--
--   - none:
--       ignore: true
--
--   - name: C
--     out-file: exam_C.qmd
--
-- The older top-level map schema
--
--   versions:
--     ignore: none
--     items:
--       - A
--
-- is intentionally not supported by `parse_versions_config` below.
local function parse_version_list_item(item)
  if not is_map(item) then
    local name = meta_to_string(item)

    if name and name ~= '' then
      get_or_create_version_spec(name)
    end

    return
  end

  -- Alternate list-item spelling, still within the list-style schema:
  --
  --   versions:
  --     - name: A
  --       out-file: exam_A.qmd
  --
  -- This is useful when a version name would otherwise collide with a YAML key
  -- or when the user prefers a uniform map shape.
  local explicit_name = map_get(item, { 'name', 'version' })

  if explicit_name ~= nil then
    local name = meta_to_string(explicit_name)

    if opts_mark_ignore(item) then
      set_ignore_label(name)
    else
      local spec = get_or_create_version_spec(name)
      apply_options_to_spec(spec, item)
    end

    return
  end

  -- Main spelling for configured versions and ignore tags:
  --
  --   - A:
  --       out-dir: ../student
  --
  --   - none:
  --       ignore: true
  for k, v in pairs(item) do
    if not RESERVED_VERSION_KEYS[k] then
      if opts_mark_ignore(v) then
        set_ignore_label(k)
      else
        local spec = get_or_create_version_spec(k)
        apply_options_to_spec(spec, v)
      end
    end
  end
end

-- Convert one YAML `versions:` list item into one or more normalized entries.
--
-- This helper does not yet decide whether a label such as `A-solution` is a
-- main version named `A-solution` or a subversion named `solution` under main
-- version `A`.  That decision needs to see the entire list, so it happens in
-- `parse_version_list` below.
local function entries_from_version_list_item(item)
  local entries = {}

  -- Plain scalar item:
  --
  --   versions:
  --     - A
  if not is_map(item) then
    local name = meta_to_string(item)

    if name and trim(name) ~= '' then
      table.insert(entries, {
        name = trim(name),
        opts = nil,
        ignore = false
      })
    end

    return entries
  end

  -- Uniform map item:
  --
  --   versions:
  --     - name: A
  --       out-file: exam_A.qmd
  local explicit_name = map_get(item, { 'name', 'version' })

  if explicit_name ~= nil then
    local name = trim(meta_to_string(explicit_name) or '')

    if name ~= '' then
      table.insert(entries, {
        name = name,
        opts = item,
        ignore = opts_mark_ignore(item)
      })
    end

    return entries
  end

  -- Main configured spelling:
  --
  --   versions:
  --     - A:
  --         out-file: exam_A.qmd
  --
  -- and ignore spelling:
  --
  --     - none:
  --         ignore: true
  for k, v in pairs(item) do
    if not RESERVED_VERSION_KEYS[k] then
      local name = trim(k)

      if name ~= '' then
        table.insert(entries, {
          name = name,
          opts = v,
          ignore = opts_mark_ignore(v)
        })
      end
    end
  end

  return entries
end

-- Return a flat list of normalized entries from the YAML `versions:` sequence.
local function collect_version_entries(xs)
  local entries = {}

  for _, item in ipairs(xs or {}) do
    for _, entry in ipairs(entries_from_version_list_item(item)) do
      table.insert(entries, entry)
    end
  end

  return entries
end

-- Determine which configured labels are main versions.
--
-- Rule:
--   If a non-ignore label has another non-ignore label as a hyphen-prefix,
--   then it is treated as a subversion of that prefix.  Otherwise it is a main
--   version.
--
-- Examples:
--   A, A-solution       => A is main; A-solution is subversion solution.
--   A-solution only     => A-solution is main, because A was not configured.
--   A, A-B, A-B-extra   => A is main; B and B-extra are subversions.
local function determine_configured_main_names(entries)
  local active_names = {}

  for _, entry in ipairs(entries or {}) do
    if not entry.ignore then
      active_names[entry.name] = true
    end
  end

  local main_names = {}

  for name, _ in pairs(active_names) do
    local has_configured_prefix = false

    for possible_prefix, _ in pairs(active_names) do
      if possible_prefix ~= name and starts_with(name, possible_prefix .. '-') then
        has_configured_prefix = true
        break
      end
    end

    if not has_configured_prefix then
      main_names[name] = true
    end
  end

  return main_names
end

-- Split a configured output label into `{main, sub}` using the main names found
-- by `determine_configured_main_names`.
local function split_configured_version_name(name, main_names)
  if main_names[name] then
    return name, nil
  end

  local best_main = nil

  for main, _ in pairs(main_names or {}) do
    if starts_with(name, main .. '-') then
      if not best_main or #main > #best_main then
        best_main = main
      end
    end
  end

  if best_main then
    return best_main, name:sub(#best_main + 2)
  end

  return name, nil
end

-- Get or create a subversion spec under a main version.
local function get_or_create_subversion_spec(main_spec, sub)
  main_spec.subversions[sub] = main_spec.subversions[sub] or {
    name = sub,
    out_dir = nil,
    out_file = nil,
    yaml = nil,
    render = nil
  }

  return main_spec.subversions[sub]
end

-- Parse a YAML sequence of version items.
local function parse_version_list(xs)
  local entries = collect_version_entries(xs)
  local main_names = determine_configured_main_names(entries)

  for _, entry in ipairs(entries) do
    if entry.ignore then
      set_ignore_label(entry.name)
    else
      local main, sub = split_configured_version_name(entry.name, main_names)
      local main_spec = get_or_create_version_spec(main)

      if sub then
        local sub_spec = get_or_create_subversion_spec(main_spec, sub)
        apply_options_to_spec(sub_spec, entry.opts)
      else
        apply_options_to_spec(main_spec, entry.opts)
      end
    end
  end
end

-- Parse the top-level `versions:` metadata from the AST.  The AST is reliable
-- for configuration, even though we avoid the AST for writing source files.
--
-- Supported top-level shape is only a YAML sequence/list:
--
--   versions:
--     - A
--     - B:
--         out-file: exam_B.qmd
--     - none:
--         ignore: true
--
-- The older map shape with `versions: {ignore, items}` is intentionally rejected
-- so configuration mistakes fail loudly instead of being silently interpreted.
local function parse_versions_config(meta)
  local v = meta.versions

  version_by_name = {}
  version_order = {}
  versions_for_matching = {}
  ignore_label = nil
  defaults = { out_dir = nil, out_file = nil, yaml = nil, render = nil }

  if not v then
    return
  end

  if is_list(v) then
    parse_version_list(v)
  else
    error('`versions:` must be a YAML list, for example:\nversions:\n  - A\n  - none:\n      ignore: true')
  end

  versions_for_matching = sort_versions_for_matching(version_order)
end

-- Remove one matching layer of single or double quotes.
local function strip_quotes(s)
  s = trim(s)

  local first = s:sub(1, 1)
  local last = s:sub(-1)

  if #s >= 2 and (first == '"' or first == "'") and first == last then
    return s:sub(2, -2)
  end

  return s
end

-- Parse a value like `A`, `A B`, `A, B`, or `[A, B]` into a list of version
-- labels.
local function labels_from_version_value(value)
  local labels = {}
  local s = strip_quotes(meta_to_string(value) or '')

  if s == '' then
    return labels
  end

  if s:sub(1, 1) == '[' and s:sub(-1) == ']' then
    s = s:sub(2, -2)
  end

  s = s:gsub('"', ''):gsub("'", '')

  for label in s:gmatch('[^,%s]+') do
    label = trim(label)

    if label ~= '' then
      table.insert(labels, label)
    end
  end

  return labels
end

-- Parse one label into main/subversion information.  For example, with main
-- version `A`, label `A-XX` becomes `{main = "A", sub = "XX"}`.
local function parse_version_label(label)
  label = trim(label)

  if ignore_label and label == ignore_label then
    return {
      raw = label,
      ignore = true,
      unknown = false
    }
  end

  if label == '' then
    return {
      raw = label,
      unknown = true
    }
  end

  for _, main in ipairs(versions_for_matching) do
    if label == main then
      return {
        raw = label,
        main = main,
        sub = nil,
        unknown = false
      }
    end

    local prefix = main .. '-'

    if starts_with(label, prefix) then
      local sub = label:sub(#prefix + 1)

      if sub ~= '' then
        return {
          raw = label,
          main = main,
          sub = sub,
          unknown = false
        }
      end
    end
  end

  return {
    raw = label,
    unknown = true
  }
end

-- Return only the text part of a line, without its trailing newline.
local function line_text(line)
  local s = tostring(line or '')

  if s:sub(-1) == '\n' then
    s = s:sub(1, -2)
  end

  if s:sub(-1) == '\r' then
    s = s:sub(1, -2)
  end

  return s
end

-- Return the original line ending for a line: `\n`, `\r\n`, or empty string.
local function line_eol(line)
  local s = tostring(line or '')

  if s:sub(-2) == '\r\n' then
    return '\r\n'
  end

  if s:sub(-1) == '\n' then
    return '\n'
  end

  return ''
end

-- Split text into lines while keeping each line's original trailing newline.
local function split_lines_keep_ends(text)
  local lines = {}
  local pos = 1

  while pos <= #text do
    local newline_pos = text:find('\n', pos, true)

    if newline_pos then
      table.insert(lines, text:sub(pos, newline_pos))
      pos = newline_pos + 1
    else
      table.insert(lines, text:sub(pos))
      break
    end
  end

  return lines
end

-- Guess the file's preferred line ending.  Generated YAML fragments use this
-- same line ending.
local function detect_eol(text)
  if text:find('\r\n', 1, true) then
    return '\r\n'
  end

  return '\n'
end

-- Return true if a YAML delimiter line starts or ends the front matter.
local function is_yaml_delimiter(line)
  local t = trim(line_text(line))
  return t == '---' or t == '...'
end

-- Split the raw source lines into YAML-front-matter lines and body lines.
local function split_front_matter(lines)
  if #lines == 0 then
    return {
      has_yaml = false,
      open = nil,
      yaml = {},
      close = nil,
      body = {}
    }
  end

  local first = line_text(lines[1])

  -- Allow a UTF-8 BOM before the opening YAML delimiter.
  first = first:gsub('^\239\187\191', '')

  if trim(first) ~= '---' then
    return {
      has_yaml = false,
      open = nil,
      yaml = {},
      close = nil,
      body = lines
    }
  end

  for i = 2, #lines do
    if is_yaml_delimiter(lines[i]) then
      local yaml_lines = {}
      local body_lines = {}

      for j = 2, i - 1 do
        table.insert(yaml_lines, lines[j])
      end

      for j = i + 1, #lines do
        table.insert(body_lines, lines[j])
      end

      return {
        has_yaml = true,
        open = lines[1],
        yaml = yaml_lines,
        close = lines[i],
        body = body_lines
      }
    end
  end

  -- If the file starts with --- but has no closing delimiter, treat everything
  -- as body rather than destroying the file.
  return {
    has_yaml = false,
    open = nil,
    yaml = {},
    close = nil,
    body = lines
  }
end

-- Recognize simple top-level YAML keys.  This intentionally targets the common
-- Quarto front-matter style: unindented keys such as `title:` or `format:`.
local function yaml_top_key(line)
  local s = line_text(line)

  if s:match('^%s') then
    return nil
  end

  return s:match('^([%w_.%-]+)%s*:')
end

-- Remove top-level YAML blocks whose keys appear in `remove_keys`.  A block is
-- the key line plus following indented/non-key lines until the next top-level
-- key.
local function remove_top_level_yaml_blocks(yaml_lines, remove_keys)
  local out = {}
  local i = 1

  while i <= #yaml_lines do
    local key = yaml_top_key(yaml_lines[i])

    if key and remove_keys[key] then
      i = i + 1

      while i <= #yaml_lines do
        local next_key = yaml_top_key(yaml_lines[i])

        if next_key then
          break
        end

        i = i + 1
      end
    else
      table.insert(out, yaml_lines[i])
      i = i + 1
    end
  end

  return out
end

-- Possible names/paths for this splitter filter.  Generated files should not
-- keep this filter in their YAML because rendering a generated file should not
-- recursively create another set of generated files.
--
-- Other filters should be preserved.  For example:
--
--   filters:
--     - versions.lua
--     - other-filter.lua
--
-- should become:
--
--   filters:
--     - other-filter.lua
local SELF_FILTER_NAMES = {
  ['versions.lua'] = true,
  ['versions'] = true
}

-- Try to discover the actual current filename, so this still works if you keep
-- the script in a different folder or rename it.  `debug.getinfo` usually
-- reports a source like `@/path/to/versions.lua` for a Lua filter.
do
  local info = debug and debug.getinfo and debug.getinfo(1, 'S') or nil

  if info and info.source then
    local src = info.source:gsub('^@', '')

    if src ~= '' then
      SELF_FILTER_NAMES[src] = true
      SELF_FILTER_NAMES[path.filename(src)] = true
    end
  end
end

-- Remove a simple trailing YAML comment from a scalar.  This is intentionally
-- conservative and is mainly for common filter-list lines such as:
--
--   - versions.lua  # create version files
local function remove_simple_yaml_comment(s)
  return trim((s or ''):gsub('%s+#.*$', ''))
end

-- Return true when one YAML scalar value points to this splitter filter.
local function is_self_filter_value(value)
  value = strip_quotes(remove_simple_yaml_comment(value))

  if value == '' then
    return false
  end

  return SELF_FILTER_NAMES[value] == true or
         SELF_FILTER_NAMES[path.filename(value)] == true
end

-- Clean a single top-level `filters:` YAML block.
--
-- The goal is to remove this splitter from the filters list, while preserving
-- every other filter exactly when possible.  If this splitter was the only
-- filter, the whole block disappears.
local function clean_filters_yaml_block(block, eol)
  local first = line_text(block[1])
  local after_colon = first:match('^[%w_.%-]+%s*:%s*(.*)$') or ''

  -- Compact scalar form:
  --
  --   filters: versions.lua
  if trim(after_colon) ~= '' and after_colon:sub(1, 1) ~= '[' then
    if is_self_filter_value(after_colon) then
      return {}
    end

    return block
  end

  -- Compact inline-list form:
  --
  --   filters: [versions.lua, other-filter.lua]
  --
  -- We rewrite this into block-list form only if this splitter appears.
  if trim(after_colon) ~= '' and after_colon:sub(1, 1) == '[' then
    local inner = trim(after_colon)
    inner = inner:sub(2, -2)

    local kept = {}
    local removed_self = false

    for item in inner:gmatch('[^,]+') do
      local value = strip_quotes(remove_simple_yaml_comment(item))

      if value ~= '' then
        if is_self_filter_value(value) then
          removed_self = true
        else
          table.insert(kept, value)
        end
      end
    end

    if not removed_self then
      return block
    end

    if #kept == 0 then
      return {}
    end

    local out = { 'filters:' .. eol }

    for _, item in ipairs(kept) do
      table.insert(out, '  - ' .. item .. eol)
    end

    return out
  end

  -- Common block-list form:
  --
  --   filters:
  --     - versions.lua
  --     - other-filter.lua
  local out = { block[1] }
  local kept_any_filter = false
  local skipping_self_item = false
  local skipped_item_indent = nil

  for i = 2, #block do
    local text = line_text(block[i])
    local indent, item = text:match('^(%s*)%-%s*(.+)%s*$')

    if item then
      if is_self_filter_value(item) then
        skipping_self_item = true
        skipped_item_indent = #indent
      else
        skipping_self_item = false
        skipped_item_indent = nil
        kept_any_filter = true
        table.insert(out, block[i])
      end
    elseif skipping_self_item then
      -- If the removed list item had continuation lines, such as options under
      -- a filter entry, remove those continuation lines too.
      local leading = text:match('^(%s*)') or ''
      local more_indented_than_removed_item = #leading > (skipped_item_indent or 0)

      if trim(text) == '' or more_indented_than_removed_item then
        -- Skip continuation/blank line belonging to the removed item.
      else
        skipping_self_item = false
        skipped_item_indent = nil
        table.insert(out, block[i])
      end
    else
      table.insert(out, block[i])
    end
  end

  if not kept_any_filter then
    return {}
  end

  return out
end

-- Remove only this splitter filter from the top-level `filters:` block.  This
-- helper runs after per-version YAML patches have been applied, so it also
-- cleans `filters:` supplied through a version-specific `yaml:` override.
local function remove_self_filter_from_yaml(yaml_lines, eol)
  local out = {}
  local i = 1

  while i <= #yaml_lines do
    local key = yaml_top_key(yaml_lines[i])

    if key == 'filters' then
      local block = { yaml_lines[i] }
      i = i + 1

      while i <= #yaml_lines do
        local next_key = yaml_top_key(yaml_lines[i])

        if next_key then
          break
        end

        table.insert(block, yaml_lines[i])
        i = i + 1
      end

      local cleaned = clean_filters_yaml_block(block, eol)

      for _, line in ipairs(cleaned) do
        table.insert(out, line)
      end
    else
      table.insert(out, yaml_lines[i])
      i = i + 1
    end
  end

  return out
end

-- Parse raw YAML front matter into Pandoc metadata.  This is used only for
-- merging per-version YAML overrides; the actual generated YAML mostly remains
-- source text.
local function parse_meta_from_yaml_lines(yaml_lines, eol)
  if #yaml_lines == 0 then
    return {}
  end

  local text = '---' .. eol .. table.concat(yaml_lines) .. '---' .. eol
  local ok, parsed = pcall(function()
    return pandoc.read(text, 'markdown')
  end)

  if ok and parsed and parsed.meta then
    return parsed.meta
  end

  log('Could not parse original YAML for metadata merging; YAML overrides may be limited.')
  return {}
end

-- Return true if a YAML line contains something other than whitespace or a
-- comment.  Used to decide whether to keep an otherwise empty YAML header.
local function has_meaningful_yaml(lines)
  for _, line in ipairs(lines or {}) do
    local t = trim(line_text(line))

    if t ~= '' and not starts_with(t, '#') then
      return true
    end
  end

  return false
end

-- Build metadata for one output target.  Merge order:
--   original YAML minus `versions`
--   defaults YAML
--   main-version YAML
--   subversion YAML
local function meta_for_target(base_meta, main_spec, sub_spec)
  local out = merged_meta(base_meta, defaults.yaml)
  out = merged_meta(out, main_spec and main_spec.yaml)

  if sub_spec then
    out = merged_meta(out, sub_spec.yaml)
  end

  return out
end

-- Add the top-level keys in one metadata map to a set.
local function add_meta_keys_to_set(set, meta)
  if not is_map(meta) then
    return
  end

  for k, _ in pairs(meta) do
    set[k] = true
  end
end

-- Return the top-level YAML keys touched by defaults/main/sub YAML overrides.
local function overlay_keys_for_target(main_spec, sub_spec)
  local keys = {}
  add_meta_keys_to_set(keys, defaults.yaml)
  add_meta_keys_to_set(keys, main_spec and main_spec.yaml)
  add_meta_keys_to_set(keys, sub_spec and sub_spec.yaml)
  return keys
end

-- Return true when a map has no keys.
local function is_empty_map(t)
  for _, _ in pairs(t or {}) do
    return false
  end

  return true
end

-- Serialize a metadata fragment to YAML lines.  Because this is only used for
-- the specific YAML keys overridden per version, it avoids serializing Quarto's
-- whole computed metadata map.
local function yaml_lines_from_meta(meta, eol)
  if not meta or is_empty_map(meta) then
    return {}
  end

  local markdown_format = 'markdown+yaml_metadata_block'

  -- Pandoc only emits YAML metadata when the markdown writer is used with a
  -- template.  Without a template, an empty document with metadata writes as
  -- an empty string, which would silently drop per-version YAML overrides.
  local markdown_template = pandoc.template.compile(pandoc.template.default('markdown'))

  local text = pandoc.write(pandoc.Pandoc({}, meta), markdown_format, {
    template = markdown_template,
    wrap_text = 'preserve'
  })
  local lines = split_lines_keep_ends(text)
  local out = {}
  local in_yaml = false

  for _, line in ipairs(lines) do
    if is_yaml_delimiter(line) then
      if not in_yaml then
        in_yaml = true
      else
        break
      end
    elseif in_yaml then
      table.insert(out, line_text(line) .. eol)
    end
  end

  return out
end

-- Build the YAML source lines for a target output.  Unaffected YAML keys are
-- copied exactly from the original source.  Only `versions` and per-version
-- overridden keys are removed/replaced.
local function yaml_for_target_source(original_yaml_lines, base_meta, main_spec, sub_spec, eol)
  local overlay_keys = overlay_keys_for_target(main_spec, sub_spec)
  local remove_keys = deep_copy(OMIT_GENERATED_YAML_KEYS)

  for k, _ in pairs(overlay_keys) do
    remove_keys[k] = true
  end

  local kept = remove_top_level_yaml_blocks(original_yaml_lines, remove_keys)
  local final_meta = meta_for_target(base_meta, main_spec, sub_spec)
  local patch_meta = {}

  for _, k in ipairs(sorted_keys(overlay_keys)) do
    if not OMIT_GENERATED_YAML_KEYS[k] and final_meta[k] ~= nil then
      patch_meta[k] = deep_copy(final_meta[k])
    end
  end

  local patch_lines = yaml_lines_from_meta(patch_meta, eol)

  if #patch_lines > 0 then
    if #kept > 0 and trim(line_text(kept[#kept])) ~= '' then
      table.insert(kept, eol)
    end

    for _, line in ipairs(patch_lines) do
      table.insert(kept, line)
    end
  end

  -- Keep all non-splitter filters, but remove this splitter filter so generated
  -- files do not recursively split themselves when rendered.
  kept = remove_self_filter_from_yaml(kept, eol)

  return kept
end

-- Extract the contents of the last `{...}` group on a line.  This is enough
-- for fenced Div/code-cell attributes such as `{.version-A}` or
-- `{r version="A"}`.
local function braced_attr_content(line)
  local text = line_text(line)
  local before, content, after = text:match('^(.-)%{(.*)%}(.*)$')

  if before == nil then
    return nil, nil, nil
  end

  return before, content, after
end

-- Return version labels from `version=...` attributes in a braced attribute
-- string.  This deliberately supports both `version="A"` and looser spacing
-- like `version = "A"`.
local function labels_from_version_attrs(content)
  local labels = {}
  local i = 1

  while i <= #content do
    local s, e = content:find('version%s*=', i)

    if not s then
      break
    end

    local before = content:sub(s - 1, s - 1)
    local boundary_ok = s == 1 or before:match('[%s{]') ~= nil

    if not boundary_ok then
      i = e + 1
    else
      local pos = e + 1

      while pos <= #content and content:sub(pos, pos):match('%s') do
        pos = pos + 1
      end

      local quote = content:sub(pos, pos)
      local value = nil
      local stop = nil

      if quote == '"' or quote == "'" then
        local close_pos = content:find(quote, pos + 1, true)

        if close_pos then
          value = content:sub(pos + 1, close_pos - 1)
          stop = close_pos + 1
        else
          value = content:sub(pos + 1)
          stop = #content + 1
        end
      else
        local value_start, value_stop = content:find('[^%s]+', pos)

        if value_start then
          value = content:sub(value_start, value_stop)
          stop = value_stop + 1
        else
          value = ''
          stop = pos
        end
      end

      for _, label in ipairs(labels_from_version_value(value)) do
        table.insert(labels, label)
      end

      i = stop
    end
  end

  return labels
end

-- Collect version labels from classes such as `.version-A` inside a braced
-- attribute string.
local function labels_from_version_classes(content)
  local labels = {}

  for cls in content:gmatch('%.([%w_.:%-]+)') do
    if starts_with(cls, 'version-') then
      table.insert(labels, cls:sub(#'version-' + 1))
    end
  end

  return labels
end

-- Collect parsed version markers from a braced attribute string.
local function markers_from_attr_content(content)
  local markers = {}

  if not content then
    return markers
  end

  for _, label in ipairs(labels_from_version_classes(content)) do
    table.insert(markers, parse_version_label(label))
  end

  for _, label in ipairs(labels_from_version_attrs(content)) do
    table.insert(markers, parse_version_label(label))
  end

  return markers
end

-- Remove `version=...` attributes from a braced attribute string.
local function remove_version_attrs_from_content(content)
  local out = {}
  local i = 1

  while i <= #content do
    local s, e = content:find('version%s*=', i)

    if not s then
      table.insert(out, content:sub(i))
      break
    end

    local before = content:sub(s - 1, s - 1)
    local boundary_ok = s == 1 or before:match('[%s{]') ~= nil

    if not boundary_ok then
      table.insert(out, content:sub(i, e))
      i = e + 1
    else
      table.insert(out, content:sub(i, s - 1))

      local pos = e + 1

      while pos <= #content and content:sub(pos, pos):match('%s') do
        pos = pos + 1
      end

      local quote = content:sub(pos, pos)

      if quote == '"' or quote == "'" then
        local close_pos = content:find(quote, pos + 1, true)

        if close_pos then
          i = close_pos + 1
        else
          i = #content + 1
        end
      else
        local _, value_stop = content:find('[^%s]+', pos)

        if value_stop then
          i = value_stop + 1
        else
          i = pos
        end
      end
    end
  end

  return table.concat(out)
end

-- Remove version classes and version attributes from a braced attribute string,
-- then normalize whitespace inside the braces.
local function strip_version_markers_from_attr_content(content)
  content = remove_version_attrs_from_content(content)
  content = content:gsub('%.version%-[%w_.:%-]+', '')
  content = trim(content:gsub('%s+', ' '))
  return content
end

-- Replace a line's braced attribute content.  If the replacement content is
-- empty and `allow_no_braces` is true, the entire `{...}` group is removed.
local function replace_braced_attr_content(line, new_content, allow_no_braces)
  local eol = line_eol(line)
  local text = line_text(line)
  local before, _, after = text:match('^(.-)%{(.*)%}(.*)$')

  if before == nil then
    return line
  end

  if new_content == '' and allow_no_braces then
    return before .. after .. eol
  end

  return before .. '{' .. new_content .. '}' .. after .. eol
end

-- Return true if the line starts a fenced code block.
local function code_fence_info(line)
  local text = line_text(line)
  local indent, ticks, rest = text:match('^(%s*)(`+)(.*)$')

  if ticks and #ticks >= 3 then
    return {
      indent = indent,
      char = '`',
      len = #ticks,
      rest = rest
    }
  end

  local tildes = nil
  indent, tildes, rest = text:match('^(%s*)(~+)(.*)$')

  if tildes and #tildes >= 3 then
    return {
      indent = indent,
      char = '~',
      len = #tildes,
      rest = rest
    }
  end

  return nil
end

-- Return true if a line closes the current fenced code block.
local function is_code_fence_close(line, info)
  local t = trim(line_text(line))
  local seq = nil

  if info.char == '`' then
    seq = t:match('^(`+)$')
  else
    seq = t:match('^(~+)$')
  end

  return seq ~= nil and #seq >= info.len
end

-- Collect a whole fenced code block starting at line `i`.
local function collect_code_block(lines, i, info)
  local block = { lines[i] }
  local j = i + 1

  while j <= #lines do
    table.insert(block, lines[j])

    if is_code_fence_close(lines[j], info) then
      return block, j + 1
    end

    j = j + 1
  end

  return block, j
end

-- Return true if a source line is a fenced Div opener.
local function div_opener_info(line)
  local text = line_text(line)
  local indent, colons, rest = text:match('^(%s*)(:+)(.*)$')

  if not colons or #colons < 3 then
    return nil
  end

  -- A bare line of colons is treated as a closer, not an opener.
  if trim(rest) == '' then
    return nil
  end

  return {
    indent = indent,
    len = #colons,
    rest = rest
  }
end

-- Return true if a line closes a fenced Div of length `len` or shorter.
local function is_div_close(line, len)
  local t = trim(line_text(line))
  local seq = t:match('^(:+)$')
  return seq ~= nil and #seq >= len
end

-- Return the body of a Quarto code-cell option line, or nil if the line is
-- not a cell-option line.
--
-- For example, this source line:
--
--   #| echo: false
--
-- returns `echo: false`.  For an indented YAML-list continuation such as:
--
--   #|   - A-solution
--
-- it returns `  - A-solution`.  Keeping that indentation lets us distinguish
-- a continuation line from the next top-level option.
local function code_option_body(line)
  -- Avoid a compact pattern such as `^%s*#| ?(.*)$` here.  It is easy to
  -- accidentally lose the indentation that tells us whether a following line is
  -- a YAML continuation of the previous option.  The manual scan below removes
  -- exactly one optional space after `#|`, matching Quarto's visual style, but
  -- preserves any additional indentation.
  local text = line_text(line)

  -- Accept both Quarto's normal `#|` prefix and the occasional hand-written
  -- `# |` variant.  Lua patterns do not treat `|` specially, but `%s*` lets us
  -- tolerate that extra space after `#`.
  local _, stop = text:find('^%s*#%s*|')

  if not stop then
    return nil
  end

  local body = text:sub(stop + 1)

  -- Quarto commonly writes `#| echo: false`; drop that one visual separator
  -- space/tab.  If the line is `#|    - A`, this leaves `   - A`, which we
  -- need so the line is treated as a continuation rather than a new top-level
  -- option.
  if body:sub(1, 1):match('%s') then
    body = body:sub(2)
  end

  return body
end

-- Return true when a cell-option body starts a new top-level option.
--
-- `version: A` and `echo: false` are top-level option lines.
-- `  - A` is not; it is a YAML-list continuation from the previous option.
local function is_top_level_code_option_body(body)
  if body == nil then
    return false
  end

  -- YAML-list continuation lines such as `#|    - A` are not new options.
  -- Check this after trimming because both `#| - A` and `#|    - A` should be
  -- treated as continuations of the previous option.
  if trim(body):match('^%-') then
    return false
  end

  -- Most Quarto option lines are written as `#| key: value`, so after the
  -- optional visual separator following `#|` the key begins immediately.  Allow
  -- a small amount of extra padding so hand-aligned forms like
  -- `#|    version: A` still work.  Very indented `key: value` lines are more
  -- likely to be YAML continuations of a previous option.
  local leading = body:match('^(%s*)') or ''

  if #leading > 3 then
    return false
  end

  return body:match('^%s*[%w_.%-]+%s*[:=]') ~= nil
end

-- Return true if a code-option body is the start of a `version` option.
local function is_version_code_option_body(body)
  if body == nil then
    return false
  end

  -- Accept both `#| version: A` and slightly padded forms such as
  -- `#|    version: A`.
  return body:match('^%s*version%s*[:=]') ~= nil
end

-- Return the value written on the same line as a `version` code option.
--
-- It returns:
--   * `A` for `version: A`
--   * `[A, B]` for `version: [A, B]`
--   * an empty string for `version:` so the caller can look for continuation
--     lines such as `#|   - A`.
local function inline_version_value_from_code_option_body(body)
  if body == nil then
    return nil
  end

  local value = body:match('^%s*version%s*:%s*(.*)$')

  if value == nil then
    value = body:match('^%s*version%s*=%s*(.*)$')
  end

  return value
end

-- Extract a version label from one YAML-list continuation body.
--
-- This handles the common multi-line Quarto option form:
--
--   #| version:
--   #|   - A-solution
--   #|   - B-solution
local function labels_from_version_continuation_body(body)
  local labels = {}
  local t = trim(body or '')

  if t == '' or starts_with(t, '#') then
    return labels
  end

  -- Convert a YAML list item like `- A-solution` to `A-solution`.
  t = t:gsub('^%-%s*', '')

  for _, label in ipairs(labels_from_version_value(t)) do
    table.insert(labels, label)
  end

  return labels
end

-- Return true if source line `idx` should be skipped because it is part of a
-- `#| version:` option block.  This removes both the option line itself and
-- YAML-list continuation lines underneath it.
local function is_version_code_option_block_line(block, idx)
  local body = code_option_body(block[idx])

  if not is_version_code_option_body(body) then
    return false
  end

  return true
end

-- Find the index of the first line after a `#| version:` option block.
--
-- This deliberately removes the whole YAML value attached to `version:`.  That
-- includes the common multi-line Quarto option form:
--
--   #| version:
--   #|    - A-solution
--   #|    - B-solution
--
-- It stops at the next top-level cell option, such as `#| echo: false`, or at
-- the first non-`#|` source line, which is the beginning of real code.
local function next_i_after_version_code_option_block(block, idx)
  local j = idx + 1

  while j < #block do
    local body = code_option_body(block[j])

    -- Non-option line: code has started, so the version option block is done.
    if body == nil then
      break
    end

    -- A new top-level option starts a separate option block and should be kept.
    if is_top_level_code_option_body(body) then
      break
    end

    -- Otherwise this is a continuation of the version value.  This covers
    -- blank `#|` lines, indented YAML, and YAML list items like `#| - A`.
    j = j + 1
  end

  return j
end

-- Extract version labels from Quarto `#| version:` cell options in a code block.
--
-- Supported forms include:
--
--   #| version: A
--   #| version: [A, B]
--   #| version:
--   #|   - A
--   #|   - B
local function labels_from_code_options(block)
  local labels = {}
  local i = 2

  -- Skip the opening fence at index 1 and the closing fence at the end.
  while i < #block do
    local body = code_option_body(block[i])

    if body == nil then
      -- Quarto cell options live at the top of the cell; stop at real code.
      break
    end

    if trim(body) == '' then
      -- Initial blank option lines are harmless.
      i = i + 1
    elseif is_version_code_option_body(body) then
      local inline_value = inline_version_value_from_code_option_body(body) or ''

      if trim(inline_value) ~= '' then
        for _, label in ipairs(labels_from_version_value(inline_value)) do
          table.insert(labels, label)
        end
      else
        -- Multi-line version option.  Keep reading indented continuation lines
        -- until another top-level option such as `echo:` begins.
        local j = i + 1

        while j < #block do
          local continuation_body = code_option_body(block[j])

          if continuation_body == nil then
            break
          end

          if is_top_level_code_option_body(continuation_body) then
            break
          end

          for _, label in ipairs(labels_from_version_continuation_body(continuation_body)) do
            table.insert(labels, label)
          end

          j = j + 1
        end
      end

      i = next_i_after_version_code_option_block(block, i)
    elseif is_top_level_code_option_body(body) then
      -- Some other option, e.g. `echo: false`; keep scanning options.
      i = i + 1
    elseif body:match('^%s') then
      -- Continuation of some other option.
      i = i + 1
    else
      break
    end
  end

  return labels
end

-- Collect parsed version markers from a fenced code block.
local function markers_from_code_block(block)
  local markers = {}
  local _, content = braced_attr_content(block[1])

  for _, marker in ipairs(markers_from_attr_content(content)) do
    table.insert(markers, marker)
  end

  for _, label in ipairs(labels_from_code_options(block)) do
    table.insert(markers, parse_version_label(label))
  end

  return markers
end

-- Collect parsed version markers from a fenced Div opener line.
local function markers_from_div_opener(line)
  local _, content = braced_attr_content(line)
  return markers_from_attr_content(content)
end

-- Return true if any marker is the configured ignore marker.
local function has_ignore_marker(markers)
  for _, marker in ipairs(markers or {}) do
    if marker.ignore then
      return true
    end
  end

  return false
end

-- Return true if a parsed marker should appear in the target output.
local function marker_matches_target(marker, target)
  if marker.ignore or marker.unknown or not marker.main then
    return false
  end

  if marker.main ~= target.main then
    return false
  end

  if target.sub then
    -- Subversion files receive main-version content plus matching subversion
    -- content.  For A/XX, keep A and A-XX.
    return marker.sub == nil or marker.sub == target.sub
  end

  if INCLUDE_SUBVERSIONS_IN_MAIN then
    return true
  end

  -- Main version files receive only untagged content plus exact main-version
  -- content.  For A, keep A but not A-XX.
  return marker.sub == nil
end

-- Decide whether an element with the given markers should be kept.  Untagged
-- elements are kept everywhere.  Ignore markers win over all other markers.
local function markers_match_target(markers, target)
  if #markers == 0 then
    return true
  end

  if has_ignore_marker(markers) then
    return false
  end

  for _, marker in ipairs(markers) do
    if marker_matches_target(marker, target) then
      return true
    end
  end

  return false
end

-- Strip version markers from a kept Div opener.  If no attributes remain,
-- return `nil` to signal that the Div should be unwrapped.
local function stripped_div_opener_or_nil(line)
  if not STRIP_VERSION_MARKERS then
    return line
  end

  local _, content = braced_attr_content(line)

  if not content then
    return line
  end

  local stripped = strip_version_markers_from_attr_content(content)

  if stripped == '' then
    return nil
  end

  return replace_braced_attr_content(line, stripped, false)
end

-- Return true when a code-option continuation line looks like an orphaned
-- version-list item, e.g. `#|    - A-solution`.
--
-- This is a defensive cleanup for files where the `#| version:` line was
-- removed but continuation lines were left behind by unusual spacing.  To avoid
-- deleting unrelated list-valued options, a line is removed only when every list
-- item on the line parses as a known version/subversion or as the ignore label.
local function is_orphan_version_list_option_line(line)
  local body = code_option_body(line)

  if body == nil then
    return false
  end

  local t = trim(body)

  if not t:match('^%-') then
    return false
  end

  local labels = labels_from_version_continuation_body(body)

  if #labels == 0 then
    return false
  end

  for _, label in ipairs(labels) do
    local marker = parse_version_label(label)

    if marker.unknown or (not marker.ignore and not marker.main) then
      return false
    end
  end

  return true
end

-- Remove orphaned version-list option lines that may have survived a previous
-- strip pass.  The function scans only the cell-option preamble at the top of a
-- code block and preserves continuation lines belonging to a non-version option.
local function remove_orphan_version_list_option_lines(block)
  local out = {}
  local in_option_preamble = true

  for i, line in ipairs(block or {}) do
    if i == 1 or i == #block then
      -- Always keep the opening and closing code fences.  This helper only
      -- cleans Quarto `#|` option lines inside the cell.
      table.insert(out, line)
    elseif not in_option_preamble then
      -- Once real code has started, never delete lines.  A line such as
      -- `#| - A` could be literal code/comment text after the preamble.
      table.insert(out, line)
    else
      local body = code_option_body(line)

      if body == nil then
        -- First non-option line: this is the start of actual code.
        in_option_preamble = false
        table.insert(out, line)
      elseif is_orphan_version_list_option_line(line) then
        -- Drop stale continuation lines such as:
        --   #|    - B-solution
        --   #|    - A-solution
        --
        -- The earlier implementation kept these when another option such as
        -- `#| echo: false` appeared before `#| version:`.  In that case the
        -- orphaned list lines were incorrectly treated as continuations of the
        -- previous non-version option.  Here we remove any top-of-cell list
        -- item whose values all parse as configured version labels.
      else
        table.insert(out, line)
      end
    end
  end

  return out
end

-- Strip version markers from a kept code block's opener and remove `#| version`
-- option blocks from the block body.
local function stripped_code_block(block)
  if not STRIP_VERSION_MARKERS then
    return block
  end

  local out = {}
  local i = 1

  while i <= #block do
    local line = block[i]

    if i == 1 then
      -- Opening fence, e.g. ```{r version="A"}.  Remove version=... from the
      -- attribute braces, but keep the engine and all non-version attributes.
      local _, content = braced_attr_content(line)

      if content then
        local stripped = strip_version_markers_from_attr_content(content)
        table.insert(out, replace_braced_attr_content(line, stripped, true))
      else
        table.insert(out, line)
      end

      i = i + 1
    elseif i < #block then
      local body = code_option_body(line)

      if is_version_code_option_body(body) then
        -- Remove the whole `#| version:` option, including indented YAML-list
        -- continuation lines such as:
        --   #| version:
        --   #|    - B-solution
        --   #|    - A-solution
        i = next_i_after_version_code_option_block(block, i)
      elseif is_orphan_version_list_option_line(line) then
        -- Defensive cleanup for a malformed/intermediate state where the
        -- `#| version:` line is already gone but its list items remain at the
        -- top of the cell.
        i = i + 1
      else
        table.insert(out, line)
        i = i + 1
      end
    else
      -- Closing fence.
      table.insert(out, line)
      i = i + 1
    end
  end

  -- Run the defensive cleanup again after the main pass, because removing a
  -- `#| version:` option can make its former continuation lines adjacent to a
  -- previous non-version option.
  out = remove_orphan_version_list_option_lines(out)

  -- If the only cell option was `version`, removing it can leave a blank line
  -- immediately after the opening fence:
  --   ```{r}
  --
  --   code
  --   ```
  -- That blank line was usually just the separator between cell options and code,
  -- so remove leading blank body lines after the opener.  Blank lines later in the
  -- code block are left untouched.
  while #out > 2 and trim(line_text(out[2])) == '' do
    table.remove(out, 2)
  end

  return out
end

-- Append all lines from `src` into `dest`.
local function append_lines(dest, src)
  for _, line in ipairs(src or {}) do
    table.insert(dest, line)
  end
end

-- Recursively filter source lines.  If `stop_div_len` is non-nil, this function
-- returns when it reaches the matching Div close line; the caller decides
-- whether to copy that close line.
local function filter_lines_recursive(lines, start_i, target, stop_div_len)
  local out = {}
  local i = start_i

  while i <= #lines do
    if stop_div_len and is_div_close(lines[i], stop_div_len) then
      return out, i
    end

    local code_info = code_fence_info(lines[i])

    if code_info then
      local block, next_i = collect_code_block(lines, i, code_info)
      local markers = markers_from_code_block(block)

      if markers_match_target(markers, target) then
        append_lines(out, stripped_code_block(block))
      end

      i = next_i
    else
      local div_info = div_opener_info(lines[i])

      if div_info then
        local markers = markers_from_div_opener(lines[i])
        local keep_div = markers_match_target(markers, target)
        local opener = nil

        if keep_div then
          opener = stripped_div_opener_or_nil(lines[i])

          if opener then
            table.insert(out, opener)
          end
        end

        local inner, close_i = filter_lines_recursive(lines, i + 1, target, div_info.len)

        if keep_div then
          append_lines(out, inner)

          if close_i <= #lines and is_div_close(lines[close_i], div_info.len) then
            if opener then
              table.insert(out, lines[close_i])
            end
          end
        end

        if close_i <= #lines and is_div_close(lines[close_i], div_info.len) then
          i = close_i + 1
        else
          i = close_i
        end
      else
        table.insert(out, lines[i])
        i = i + 1
      end
    end
  end

  return out, i
end

-- Return true if a line is visually blank.
local function is_blank_line(line)
  return trim(line_text(line)) == ''
end

-- Collapse long runs of blank lines after versioned material has been removed.
--
-- This keeps ordinary Markdown paragraph spacing: at most one blank line
-- between retained pieces of content.  Set MAX_CONSECUTIVE_BLANK_LINES above
-- to 2 or nil if you want looser preservation of vertical whitespace.
--
-- The function deliberately does not collapse blank lines inside fenced code
-- blocks.  Blank lines inside code are program text; blank lines outside code are
-- markdown spacing left behind when versioned blocks are removed.
local function collapse_blank_runs(lines)
  if not MAX_CONSECUTIVE_BLANK_LINES then
    return lines
  end

  local out = {}
  local blank_count = 0
  local active_code_fence = nil

  for _, line in ipairs(lines or {}) do
    if active_code_fence then
      -- Preserve code text exactly, including blank lines inside code blocks.
      table.insert(out, line)

      if is_code_fence_close(line, active_code_fence) then
        active_code_fence = nil
      end
    else
      local info = code_fence_info(line)

      if info then
        table.insert(out, line)
        active_code_fence = info
        blank_count = 0
      elseif is_blank_line(line) then
        blank_count = blank_count + 1

        if blank_count <= MAX_CONSECUTIVE_BLANK_LINES then
          table.insert(out, line)
        end
      else
        blank_count = 0
        table.insert(out, line)
      end
    end
  end

  -- Removed versioned blocks at the beginning or end of a document often leave
  -- leading/trailing empty space.  Drop those outer body-only blanks while still
  -- preserving blank lines inside fenced code blocks above.
  while #out > 0 and is_blank_line(out[1]) do
    table.remove(out, 1)
  end

  while #out > 0 and is_blank_line(out[#out]) do
    table.remove(out)
  end

  return out
end

-- Public wrapper for filtering the document body.
local function filter_body_lines(lines, target)
  local out, _ = filter_lines_recursive(lines, 1, target, nil)
  return collapse_blank_runs(out)
end

-- Record any subversions found in a list of markers.
local function record_subversions_from_markers(subs, markers)
  for _, marker in ipairs(markers or {}) do
    if marker.main and marker.sub and not marker.ignore then
      subs[marker.main][marker.sub] = true
    end
  end
end

-- Discover subversions by scanning the original source body.  This determines
-- which subdirectories such as A/XX should be written.
local function discover_subversions_from_source(body_lines)
  local subs = {}

  for _, v in ipairs(version_order) do
    subs[v] = {}
  end

  local i = 1

  while i <= #body_lines do
    local code_info = code_fence_info(body_lines[i])

    if code_info then
      local block, next_i = collect_code_block(body_lines, i, code_info)
      record_subversions_from_markers(subs, markers_from_code_block(block))
      i = next_i
    else
      local div_info = div_opener_info(body_lines[i])

      if div_info then
        record_subversions_from_markers(subs, markers_from_div_opener(body_lines[i]))
      end

      i = i + 1
    end
  end

  -- Also write subversions explicitly configured in YAML, even if the source
  -- currently contains no A-XX block.
  for _, main in ipairs(version_order) do
    local spec = version_by_name[main]

    if spec and spec.subversions then
      for sub, _ in pairs(spec.subversions) do
        subs[main][sub] = true
      end
    end
  end

  return subs
end

-- Resolve output directory and filename for a target version/subversion.
local function output_path_for_target(target, source_file)
  local main_spec = version_by_name[target.main]
  local sub_spec = nil

  if target.sub and main_spec and main_spec.subversions then
    sub_spec = main_spec.subversions[target.sub]
  end

  -- Resolve an output root, then make sure the main version folder is present.
  -- This prevents a plain `out-dir: .` from overwriting the source file and
  -- keeps the generated structure stable:
  --
  --   A/source.qmd
  --   A/solution/source.qmd
  --
  -- If the configured path already ends in the main version name, it is used as
  -- is.  Thus both `out-dir: generated` and `out-dir: generated/A` work.
  local root_dir = nil

  if main_spec and main_spec.out_dir then
    root_dir = resolve_dir(apply_placeholders(main_spec.out_dir, target, source_file))
  elseif defaults.out_dir then
    root_dir = resolve_dir(apply_placeholders(defaults.out_dir, target, source_file))
  else
    root_dir = system.get_working_directory()
  end

  local main_dir = append_path_part_if_missing(root_dir, target.main)
  local dir = main_dir

  if target.sub then
    if sub_spec and sub_spec.out_dir then
      local configured_sub_dir = apply_placeholders(sub_spec.out_dir, target, source_file)

      if path.is_absolute(configured_sub_dir) then
        -- Absolute subversion paths are treated as deliberate full overrides,
        -- but we still append the subversion folder if it is not already there.
        dir = append_path_part_if_missing(path.normalize(configured_sub_dir), target.sub)
      else
        -- Relative subversion paths are relative to the main version directory,
        -- not to the current working directory.  This is the key fix for
        -- `A-solution` folders escaping outside `A/`.
        dir = append_path_part_if_missing(
          path.normalize(path.join({ main_dir, configured_sub_dir })),
          target.sub
        )
      end
    else
      dir = append_path_part_if_missing(main_dir, target.sub)
    end
  end

  local configured_file = nil

  if sub_spec and sub_spec.out_file then
    configured_file = sub_spec.out_file
  elseif main_spec and main_spec.out_file then
    configured_file = main_spec.out_file
  elseif defaults.out_file then
    configured_file = defaults.out_file
  else
    configured_file = source_file
  end

  local file = safe_file_name(apply_placeholders(configured_file, target, source_file))

  return dir, path.normalize(path.join({ dir, file })), main_spec, sub_spec
end

-- Assemble a final `.qmd` from YAML lines and body lines.
local function assemble_qmd(front, yaml_lines, body_lines, eol)
  local out = {}

  if front.has_yaml or has_meaningful_yaml(yaml_lines) then
    if has_meaningful_yaml(yaml_lines) then
      table.insert(out, front.open or ('---' .. eol))
      append_lines(out, yaml_lines)
      table.insert(out, front.close or ('---' .. eol))
    end
  end

  append_lines(out, body_lines)
  return table.concat(out)
end

-- Decide whether a generated file should be rendered after it is written.
--
-- Resolution order is defaults -> main version -> subversion, so a subversion
-- can override its main version.  Anything other than explicit true is treated
-- as false.
local function should_render_target(main_spec, sub_spec)
  local render = defaults.render

  if main_spec and main_spec.render ~= nil then
    render = main_spec.render
  end

  if sub_spec and sub_spec.render ~= nil then
    render = sub_spec.render
  end

  return render == true
end

-- Quote one path for the user's shell before calling `quarto render`.
-- On macOS/Linux, single-quote wrapping is robust even for spaces.  On Windows,
-- double-quote wrapping is the least surprising option for ordinary paths.
local function shell_quote(s)
  s = tostring(s or '')

  if package.config:sub(1, 1) == [[\]] then
    return '"' .. s:gsub([["]], [[\"]]) .. '"'
  end

  return "'" .. s:gsub("'", [['"'"']]) .. "'"
end

-- Render a generated `.qmd` with the Quarto CLI.
--
-- The generated YAML removes this splitter from `filters`, which prevents the
-- render from recursively invoking the splitter while preserving any other
-- filters declared in the original document YAML.
local function render_qmd_file(outfile)
  log('rendering ' .. outfile)

  local command = 'quarto render ' .. shell_quote(outfile)
  local ok, how, code = os.execute(command)

  if ok ~= true and ok ~= 0 then
    error(
      'quarto render failed for ' .. outfile ..
      ' with command `' .. command .. '`' ..
      ' (status: ' .. tostring(ok) .. ', ' .. tostring(how) .. ', ' .. tostring(code) .. ')'
    )
  end
end

-- Write one source-preserved `.qmd` for one target version/subversion.
local function write_source_version_doc(front, base_meta, target, source_file, source_path, eol)
  local dir, outfile, main_spec, sub_spec = output_path_for_target(target, source_file)

  if same_normalized_path(outfile, source_path) then
    error(
      'Refusing to overwrite the source file while writing version `' ..
      tostring(target.main) .. (target.sub and ('-' .. tostring(target.sub)) or '') ..
      '`. Computed output path was ' .. outfile ..
      '. Set `out-dir` so generated files live outside the source location.'
    )
  end

  ensure_dir(dir)

  local yaml_lines = yaml_for_target_source(front.yaml, base_meta, main_spec, sub_spec, eol)
  local body_lines = filter_body_lines(front.body, target)
  local text = assemble_qmd(front, yaml_lines, body_lines, eol)

  if text ~= '' and text:sub(-1) ~= '\n' then
    text = text .. eol
  end

  write_text_file(outfile, text)
  log('wrote ' .. outfile)

  if should_render_target(main_spec, sub_spec) then
    render_qmd_file(outfile)
  end
end

-- Main document-level filter.  Pandoc/Quarto calls this once with the complete
-- parsed document.  We use `doc.meta` to read the version configuration, then
-- read and filter the original source text.
function Pandoc(doc)
  parse_versions_config(doc.meta)

  if #version_order == 0 then
    log('No YAML `versions` list found; nothing written.')
    return doc
  end

  local source_path = get_source_file_path()

  if not source_path then
    error('Could not determine the original source file path.')
  end

  local source_file = get_source_file_name(source_path)
  local source_text = read_text_file(source_path)
  local eol = detect_eol(source_text)
  local source_lines = split_lines_keep_ends(source_text)
  local front = split_front_matter(source_lines)

  local base_yaml_without_versions = remove_top_level_yaml_blocks(front.yaml, OMIT_GENERATED_YAML_KEYS)
  local base_meta = parse_meta_from_yaml_lines(base_yaml_without_versions, eol)
  local subversions = discover_subversions_from_source(front.body)

  for _, main in ipairs(version_order) do
    write_source_version_doc(
      front,
      base_meta,
      { main = main, sub = nil },
      source_file,
      source_path,
      eol
    )

    for _, sub in ipairs(sorted_keys(subversions[main])) do
      write_source_version_doc(
        front,
        base_meta,
        { main = main, sub = sub },
        source_file,
        source_path,
        eol
      )
    end
  end

  return doc
end
