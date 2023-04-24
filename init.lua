cosnoise = {}
local pi = math.pi
local floor = math.floor
local ceil = math.ceil
local random = math.random

-- Generate cosine and sine lookup tables for speed

-- Precompute sine values for angles between 0 and 2 * pi with 1024 elements
local sine_table = {}
for i = 0, 1023 do
    sine_table[i] = math.sin(2 * pi * i / 1024)
end

-- Function to normalize any input angle to the range 0 to 2 * pi
local function normalize_angle(angle)
    local two_pi = 2 * pi
    return angle - two_pi * math.floor(angle / two_pi)
end

-- Function to find the nearest table index for a given angle
local function find_nearest_index(angle)
    return math.floor(normalize_angle(angle) * 1024 / (2 * pi)) % 1024
end

-- Sine function using precomputed table and linear interpolation
local function sin(angle)
    local index = find_nearest_index(angle)
    local alpha = normalize_angle(angle) * 1024 / (2 * pi) - index

    local index_next = (index + 1) % 1024

    return sine_table[index] * (1 - alpha) + sine_table[index_next] * alpha
end

-- Cosine function using precomputed sine table and linear interpolation
local function cos(angle)
    return sin(angle + pi / 2)
end


local function randomize_list(list)
    local idxs = {}
    local newlist = {}
    for i = 1, #list do
        table.insert(idxs, i)
    end
    for i = 1, #list do 
        local index_to_remove = math.random(1, #idxs)
        local index_to_insert = table.remove(idxs, index_to_remove)
        table.insert(newlist, index_to_insert, list[i])
    end
    return newlist
end

local function create_2d_rotations(qty)
    local mats = {}
    local spacing = (2 * math.pi) / qty
    local max_offset = spacing / 3
    local offset = function(max)
        return (math.random() - 0.5) * max
    end
    for i = 1, qty do
        local angle = i * spacing + offset(max_offset)
        local mat = {{math.cos(angle), -math.sin(angle)}, 
                     {math.sin(angle),  math.cos(angle)}}
        table.insert(mats, mat)
    end
    return randomize_list(mats)
end

local function create_3d_rotations(qty)
    local mats = {}
    local spacing = math.pi * (3 - math.sqrt(5)) -- golden angle
    local max_offset = spacing / 3
    local offset = function(max)
        return (math.random() - 0.5) * max
    end
    local phi = math.pi * (3 - math.sqrt(5)) -- start with golden angle
    for i = 1, qty do
        local y = 1 - (i - 1) / (qty - 1) * 2 -- map i to [-1,1]
        local radius = math.sqrt(1 - y * y)
        local theta = i * spacing + offset(max_offset)
        local x = radius * math.cos(theta)
        local z = radius * math.sin(theta)
        local mat = {
            {x * x + (1 - x * x) * math.cos(phi), x * y * (1 - math.cos(phi)) - z * math.sin(phi), x * z * (1 - math.cos(phi)) + y * math.sin(phi)},
            {x * y * (1 - math.cos(phi)) + z * math.sin(phi), y * y + (1 - y * y) * math.cos(phi), y * z * (1 - math.cos(phi)) - x * math.sin(phi)},
            {x * z * (1 - math.cos(phi)) - y * math.sin(phi), y * z * (1 - math.cos(phi)) + x * math.sin(phi), z * z + (1 - z * z) * math.cos(phi)}
        }
        table.insert(mats, mat)
        phi = phi + spacing
    end
    return randomize_list(mats)
end
  
local function create_offsets(qty)
    local offsets = {}
    for i=1,qty do
        table.insert(offsets,random()*2*pi)
    end
    return offsets
end


local function rotate_2d(position, mat)
    local x_rotated = position.x * mat[1][1] + position.z * mat[1][2]
    local z_rotated = position.x * mat[2][1] + position.z * mat[2][2]
    return {x = x_rotated, z = z_rotated}
end

local function rotate_3d(position, mat)
    local x_rotated = position.x * mat[1][1] + position.y * mat[1][2] + position.z * mat[1][3]
    local y_rotated = position.x * mat[2][1] + position.y * mat[2][2] + position.z * mat[2][3]
    local z_rotated = position.x * mat[3][1] + position.y * mat[3][2] + position.z * mat[3][3]
    return {x = x_rotated, y = y_rotated, z = z_rotated}
end


function cosnoise.get_random_waves(qty,seed)
    math.randomseed(seed)
    local waves = {}
    for i=1,qty do
        table.insert(waves,random())
    end
    return waves
end

-- precomputation for each noise distribution of offests and rotations
-- pass a table of wavelengths for the cosine functions
-- returns a function that will calculate a noise value at a point.
function cosnoise.create(wavelengths,seed) 
    math.randomseed(seed)
    -- we will get more offests, one extra for each dimension, as each octave in
    -- each dimension should have a different offset than in the previous
    -- dimension.

    local offsets = create_offsets(#wavelengths+2)
    local rotations2 = create_2d_rotations(#wavelengths+2)
    local rotations3 = create_3d_rotations(#wavelengths+3)
    local waves = {}
    for i = 1, #wavelengths do 
        waves[i] = wavelengths[i] * 2 * pi
    end

    -- lets calculate the maximum value. That will be
    max = 0
    for i,v in ipairs(waves) do
        max = max + v
    end
    -- we will divide the total noise function by max


    local dim_lookup = {"x","z","y"}

    -- we will assume a minetest-like coordinate system. So, x and z are the
    -- horizantal axes. 1-d noise will look for pos = {x=x_val}. 2-d noise will
    -- look for pos = {x=x_val,z=z_val}, and 3-d noise will look for pos =
    -- {x=x_val,y=y_val,z=z_val}. Scale is a number to divide all axes by. If
    -- you want custom scale on each axis, divide each pos coordinate by your
    -- custom scale yourself. dims is an integer which specifies the number of
    -- dimensions to consider; currently supports 1, 2, or 3.

    local noise = function(pos, scale, dims)

        -- scale the noise, if needed.
        if scale ~= 1 then
            for axis,loc in pairs(pos) do
                pos[axis]=pos[axis]/scale
            end
        end

        local val = 0

        for i = 1, #waves do

            local wavelength = waves[i]

            for d = 1, dims do

                local offset = offsets[i+d-1]
                -- rotate coordinates
                if dims==2 then
                    pos = rotate_2d(pos,rotations2[i+d-1])
                end
                if dims==3 then
                    pos = rotate_3d(pos,rotations3[i+d-1])
                end
                local axis = dim_lookup[d]
                local coord = pos[axis]

                val = val + wavelength*cos((coord-offset)/wavelength)

            end

        end

        val = val / (max*dims)
        return val
    end
    return noise
end







    
