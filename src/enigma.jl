using Cairo
using Colors
using Vec
using Reel

const MONOKAI_LITE = colorant"0xCFBFADFF"
const MONOKAI_DARK = colorant"0x272822FF"
const MONOKAI_BLUE   = colorant"0x52E3F6FF"
const MONOKAI_GREEN  = colorant"0xA7EC21FF"
const MONOKAI_RED    = colorant"0xFF007FFF"
const MONOKAI_ORANGE = colorant"0xF9971FFF"
const MONOKAI_COBALT = colorant"0x79ABFFFF"

char2ind(c::Char) = uppercase(c) - 'A' + 1
ind2char(i::Int) = i + 'A' - 1

function Cairo.set_source_rgba(ctx::CairoContext, color::RGB)

    # g = convert(Float64, gray(grayscale_transform(color)))
    # r = g = b = g

    r = convert(Float64, red(color))
    g = convert(Float64, green(color))
    b = convert(Float64, blue(color))

    set_source_rgba(ctx, r, g, b, 1.0)
end
function Cairo.set_source_rgba(ctx::CairoContext, color::TransparentColor)

    # grayscale = grayscale_transform(color)
    # g = convert(Float64, gray(grayscale))
    # r = g = b = g
    # a = convert(Float64, alpha(grayscale))

    r = convert(Float64, red(color))
    g = convert(Float64, green(color))
    b = convert(Float64, blue(color))
    a = convert(Float64, alpha(color))

    set_source_rgba(ctx, r, g, b, a)
end
function Cairo.set_source_rgba(ctx::CairoContext, color₀::Colorant, color₁::Colorant, t::Real)

    r₀ = convert(Float64, red(color₀))
    g₀ = convert(Float64, green(color₀))
    b₀ = convert(Float64, blue(color₀))
    a₀ = convert(Float64, alpha(color₀))

    r₁ = convert(Float64, red(color₁))
    g₁ = convert(Float64, green(color₁))
    b₁ = convert(Float64, blue(color₁))
    a₁ = convert(Float64, alpha(color₁))

    r = r₀ + (r₁ - r₀)*t
    g = g₀ + (g₁ - g₀)*t
    b = b₀ + (b₁ - b₀)*t
    a = a₀ + (a₁ - a₀)*t
    set_source_rgba(ctx, r, g, b, a)
end

function render_text(ctx::CairoContext, text::String, x::Real, y::Real;
    font_size::Real=10.0,
    color::Colorant = colorant"black",
    font_face::String="sans-serif",
    )

    Cairo.save(ctx)
    select_font_face(ctx, font_face, Cairo.FONT_SLANT_NORMAL, Cairo.FONT_WEIGHT_NORMAL)
    set_font_size(ctx, font_size)

    set_source_rgba(ctx, color)
    move_to(ctx, x, y)
    show_text(ctx, text)
    restore(ctx)
end
function render_text_centered(ctx::CairoContext, text::String, x::Real, y::Real;
    font_size::Real=10.0,
    color::Colorant = colorant"black",
    font_face::String="sans-serif",
    )

    Cairo.save(ctx)
    select_font_face(ctx, font_face, Cairo.FONT_SLANT_NORMAL, Cairo.FONT_WEIGHT_NORMAL)
    set_font_size(ctx, font_size)
    extents = text_extents(ctx, text)

    set_source_rgba(ctx, color)
    move_to(ctx, x - extents[3]/2, y + extents[4]/2)
    show_text(ctx, text)
    restore(ctx)
end

function get_surface_and_context(canvas_width::Int, canvas_height::Int)
    s = CairoRGBSurface(canvas_width, canvas_height)
    ctx = creategc(s)
    (s, ctx)
end
function get_surface_and_context(;
    canvas_width::Int = 1000,
    canvas_height::Int = 500,
    background_color::Colorant = colorant"white",
    )

    s, ctx = get_surface_and_context(canvas_width, canvas_height)

    # fill with background color
    set_source_rgba(ctx, background_color)
    paint(ctx)

    (s, ctx)
end

bezier(A::VecE2, B::VecE2, t::Float64) = lerp(A, B, t)
bezier(A::VecE2, B::VecE2, C::VecE2, t::Float64) = (1-t)^2*A + 2*(1-t)*t*B + t*t*C
bezier(A::VecE2, B::VecE2, C::VecE2, D::VecE2, t::Float64) = (1-t)^3*A + 3*(1-t)^2*t*B + 3*(1-t)*t*t*C + t*t*t*D

type RotorRenderParams
    margin_left::Float64
    margin_top::Float64
    sep_vert::Float64
    sep_horz::Float64

    vertex_radius::Float64
    line_width::Float64

    function RotorRenderParams()
        new(200.0, 50.0, 100.0, 200.0, 10.0, 5.0)
    end
    function RotorRenderParams(margin_left::Real, margin_top::Real, sep_vert::Real, sep_horz::Real, vertex_radius::Real, line_width::Real)
        new(
            convert(Float64, margin_left),
            convert(Float64, margin_top),
            convert(Float64, sep_vert),
            convert(Float64, sep_horz),
            convert(Float64, vertex_radius),
            convert(Float64, line_width),
        )
    end
end
get_vertex_x(rp::RotorRenderParams, i::Int) = rp.margin_left + rp.sep_horz*(i-1)
get_vertex_y(rp::RotorRenderParams, j::Int) = rp.margin_top + rp.sep_vert*(j-1)
get_vertex_pos(rp::RotorRenderParams, i::Int, j::Int) = VecE2(get_vertex_x(rp, i), get_vertex_y(rp, j))

type Rotor
    enshift::Vector{Int} # amount each input gets shifted
    deshift::Vector{Int} # amount reversed inputs get shifted
    knock_points::Vector{Int}
    rotor_position::Int
    function Rotor(perm::Vector{Int}, knock_points::Vector{Int}=Int[], rotor_position::Int=0)

        n = length(perm)
        enshift = Array(Int, n)
        deshift = Array(Int, n)
        for (i,p) in enumerate(perm)
            enshift[i] = p - i
            deshift[p] = i - p
        end

        rotor_position = mod(rotor_position, n)

        new(enshift, deshift, knock_points, rotor_position)
    end
end
Base.length(rotor::Rotor) = length(rotor.enshift)
function tick!(rotor::Rotor)
    rotor.rotor_position = mod(rotor.rotor_position + 1, length(rotor))
    rotor
end
function encode(rotor::Rotor, a::Int)
    n = length(rotor.enshift) # number of letters
    a₂ = a + rotor.rotor_position
    a₂ = mod(a₂-1, n)+1
    b = a + rotor.enshift[a₂]
    mod(b-1, n)+1

    # a₂ = a - rotor.rotor_position
    # b = rotor.perm[mod(a₂-1, n)+1]
    # Δ = b - a₂
    # mod(a + Δ - 1, n)+1
end
function decode(rotor::Rotor, a::Int)
    n = length(rotor.deshift) # number of letters
    a₂ = a + rotor.rotor_position
    a₂ = mod(a₂-1, n)+1
    b = a + rotor.deshift[a₂]
    mod(b-1, n)+1
    # for b in 1 : length(rotor.enshift)
    #     if encode(rotor, b) == a
    #         return b
    #     end
    # end
    # error("decode not possible")
end

function render_bezier_edge!(ctx::CairoContext, A::VecE2, D::VecE2, color::Colorant=MONOKAI_DARK;
    line_width::Real = 5.0,
    bezier_strength::Real = 150.0,
    reflect::Bool=false,
    )

    B = A + VecE2(bezier_strength, 0.0)
    C = D + (reflect ? 1 : -1) * VecE2(bezier_strength, 0.0)

    save(ctx)
    set_source_rgba(ctx, color)
    set_line_width(ctx, line_width)
    move_to(ctx, A.x, A.y)
    curve_to(ctx, B.x, B.y, C.x, C.y, D.x, D.y)
    stroke(ctx)
    restore(ctx)

    ctx
end
function render_vertex!(ctx::CairoContext, P::VecE2, color::Colorant=MONOKAI_DARK;
    radius::Real=10.0,
    )

    save(ctx)
    set_source_rgba(ctx, color)
    translate(ctx, P.x, P.y)
    arc(ctx, 0.0, 0.0, radius, 0, 2pi)
    fill(ctx)
    restore(ctx)

    ctx
end
function render_text_and_ciphertext!(
    ctx::CairoContext,
    rotor::Rotor,
    text::String,
    cipher_text::String;
    render_params::RotorRenderParams = RotorRenderParams(),
    font_size::Real=20.0,
    only_input::Bool=false,
    font_face::String="monospace",
    )

    x = get_vertex_x(render_params, 1)
    y = get_vertex_y(render_params, length(rotor))

    render_text(ctx, text, x, y + 50, font_size=font_size, font_face=font_face, color=MONOKAI_DARK)
    if !only_input
        render_text(ctx, cipher_text, x, y + 100, font_size=font_size, font_face=font_face, color=MONOKAI_COBALT)
    end
    ctx
end
function render_text_and_ciphertext!(
    s::CairoSurface,
    rotor::Rotor,
    text::String,
    cipher_text::String;
    render_params::RotorRenderParams = RotorRenderParams(),
    font_size::Real=20.0,
    only_input::Bool=false,
    )

    ctx = creategc(s)
    render_text_and_ciphertext!(ctx, rotor, text, cipher_text, render_params=render_params, font_size=font_size, only_input=only_input)
    s
end
function render_alphabet!(
    ctx::CairoContext,
    rotor::Rotor,
    alphabet::Vector{Char};
    render_params::RotorRenderParams = RotorRenderParams(),
    font_size::Real=20.0,
    )

    for (i, c) in enumerate(alphabet)
        P = get_vertex_pos(render_params, 1, i)
        Q = get_vertex_pos(render_params, 2, i)

        render_text_centered(ctx, string(c), P.x - 50.0, P.y, font_size=font_size, color=MONOKAI_DARK)
        render_text_centered(ctx, string(c), Q.x + 50.0, Q.y, font_size=font_size, color=MONOKAI_DARK)
    end
    ctx
end
function render!(ctx::CairoContext,
    rotor::Rotor,
    active_inputs::Vector{Int},
    render_params::RotorRenderParams = RotorRenderParams(),
    )

    n = length(rotor)
    for a in 1 : n
        if !in(a, active_inputs)
            P = get_vertex_pos(render_params, 1, a)

            b = encode(rotor, a)
            Q = get_vertex_pos(render_params, 2, b)

            render_vertex!(ctx, P, MONOKAI_DARK, radius=render_params.vertex_radius)
            render_vertex!(ctx, Q, MONOKAI_DARK, radius=render_params.vertex_radius)
            render_bezier_edge!(ctx, P, Q, MONOKAI_DARK, line_width=render_params.line_width)
        end
    end
    for a in active_inputs
        P = get_vertex_pos(render_params, 1, a)

        b = encode(rotor, a)
        Q = get_vertex_pos(render_params, 2, b)

        render_vertex!(ctx, P, MONOKAI_COBALT, radius=render_params.vertex_radius)
        render_vertex!(ctx, Q, MONOKAI_COBALT, radius=render_params.vertex_radius)
        render_bezier_edge!(ctx, P, Q, MONOKAI_COBALT, line_width=render_params.line_width)
    end

    ctx
end
function render(rotor::Rotor;
    active_inputs::Vector{Int}=Int[],
    render_params::RotorRenderParams = RotorRenderParams(),
    )

    s, ctx = get_surface_and_context()
    render!(ctx, rotor, active_inputs, render_params)
    s
end

let
    rotor = Rotor([1, 2, 3, 4])
    for i in 1 : 4
        @assert encode(rotor, i) == i
        @assert decode(rotor, i) == i
    end

    tick!(rotor)
    for i in 1 : 4
        @assert encode(rotor, i) == i
        @assert decode(rotor, i) == i
    end

    rotor = Rotor([2, 1, 4, 3]) #+1  -1  +1  -1#
    @assert encode(rotor, 1) == 2 # +1
    @assert encode(rotor, 2) == 1 # -1
    @assert encode(rotor, 3) == 4 # +1
    @assert encode(rotor, 4) == 3 # -1

    @assert decode(rotor, 1) == 2
    @assert decode(rotor, 2) == 1
    @assert decode(rotor, 3) == 4
    @assert decode(rotor, 4) == 3

    tick!(rotor)
    @assert encode(rotor, 1) == 4 # (1+1 → 2) -1
    @assert encode(rotor, 2) == 3 # (2+1 → 3) +1
    @assert encode(rotor, 3) == 2 # (3+1 → 4) -1
    @assert encode(rotor, 4) == 1 # (4+1 → 1) +1

    @assert decode(rotor, 1) == 4
    @assert decode(rotor, 2) == 3
    @assert decode(rotor, 3) == 2
    @assert decode(rotor, 4) == 1

    rotor = Rotor([4, 2, 1, 3])
    @assert encode(rotor, 1) == 4
    @assert encode(rotor, 2) == 2
    @assert encode(rotor, 3) == 1
    @assert encode(rotor, 4) == 3

    @assert decode(rotor, 1) == 3
    @assert decode(rotor, 2) == 2
    @assert decode(rotor, 3) == 4
    @assert decode(rotor, 4) == 1
end

function encode(rotors::Vector{Rotor}, a::Int)
    for rotor in rotors
        a = encode(rotor, a)
    end
    a
end
function decode(rotors::Vector{Rotor}, a::Int)
    for rotor in reverse(rotors)
        a = decode(rotor, a)
    end
    a
end
function tick!(rotors::Vector{Rotor}, rotor_index::Int=1)

    # rotate the way you would expect.
    # Thus "double stepping" does not occur, as it does with the real Enigma

    rotor = rotors[rotor_index]
    if mod(rotor.rotor_position,length(rotor))+1 in rotor.knock_points && rotor_index < length(rotors)
        # rotate the next rotor as well
        tick!(rotors, rotor_index+1)
    end
    tick!(rotor) # rotate this rotor
    rotors
end
function render_alphabet!(
    ctx::CairoContext,
    rotors::Vector{Rotor},
    alphabet::Vector{Char};
    render_params::RotorRenderParams = RotorRenderParams(),
    font_size::Real=20.0,
    )

    n = length(rotors)+1
    for (i, c) in enumerate(alphabet)
        P = get_vertex_pos(render_params, 1, i)
        Q = get_vertex_pos(render_params, n, i)

        render_text_centered(ctx, string(c), P.x - 50.0, P.y, font_size=font_size, color=MONOKAI_DARK)
        render_text_centered(ctx, string(c), Q.x + 50.0, Q.y, font_size=font_size, color=MONOKAI_DARK)
    end
    ctx
end
function render_rotor_tick!(
    ctx::CairoContext,
    rotor::Rotor,
    t::Float64, # ∈ [0,1]
    color::Colorant,
    render_params::RotorRenderParams = RotorRenderParams(),
    bezier_strength::Real=125.0,
    )

    n = length(rotor)
    for a1 in 1 : n

        a2 = mod(a1-2, n)+1
        P1 = get_vertex_pos(render_params, 1, a1)
        P2 = get_vertex_pos(render_params, 1, a2)

        if a2 != n
            P = bezier(P1, P2, t)
        else # use a special bezier
            C = P1 - VecE2(0, bezier_strength)
            D = P2 + VecE2(0, bezier_strength)
            P = bezier(P1, C, D, P2, t)
        end

        b1 = encode(rotor, a1)
        b2 = mod(b1-2,n)+1
        Q1 = get_vertex_pos(render_params, 2, b1)
        Q2 = get_vertex_pos(render_params, 2, b2)

        if b2 != n
            Q = bezier(Q1, Q2, t)
        else
            C = Q1 -VecE2(0, bezier_strength)
            D = Q2 + VecE2(0, bezier_strength)
            Q = bezier(Q1, C, D, Q2, t)
        end

        render_vertex!(ctx, P, color, radius=render_params.vertex_radius)
        render_vertex!(ctx, Q, color, radius=render_params.vertex_radius)
        render_bezier_edge!(ctx, P, Q, color, line_width=render_params.line_width)
    end

    ctx
end
function render_rotor_tick!(
    ctx::CairoContext,
    rotors::Vector{Rotor},
    t::Float64, # ∈ [0,1]
    color::Colorant,
    render_params::RotorRenderParams = RotorRenderParams(),
    )

    n = length(rotors[1])
    n_rotors = length(rotors)

    left_offset = 0

    rotor_index = 1
    done = false
    while !done
        rotor = rotors[rotor_index]

        rp2 = deepcopy(render_params)
        rp2.margin_left = render_params.margin_left + render_params.sep_horz*(rotor_index-1)

        render_rotor_tick!(ctx, rotor, t, color, rp2)
        done = rotor_index == n_rotors || !in(mod(rotor.rotor_position,n)+1, rotor.knock_points)
        rotor_index += 1
    end

    while rotor_index ≤ n_rotors

        rp2 = deepcopy(render_params)
        rp2.margin_left = render_params.margin_left + render_params.sep_horz*(rotor_index-1)

        render!(ctx, rotors[rotor_index], Int[], rp2)
        rotor_index += 1
    end

    ctx
end
function render_trace!(
    ctx::CairoContext,
    rotors::Vector{Rotor},
    a::Int,
    color::Colorant,
    render_params::RotorRenderParams,
    )

    P = get_vertex_pos(render_params, 1, a)
    render_vertex!(ctx, P, color, radius=render_params.vertex_radius)

    for rotor in rotors
        b = encode(rotor, a)
        Q = VecE2(P.x + render_params.sep_horz, get_vertex_y(render_params, b))
        render_bezier_edge!(ctx, P, Q, color, line_width=render_params.line_width)
        render_vertex!(ctx, Q, color, radius=render_params.vertex_radius)
        a, P = b, Q
    end

    ctx
end
function render!(
    ctx::CairoContext,
    rotors::Vector{Rotor},
    input::Int,
    render_params::RotorRenderParams,
    )

    for a in 1 : length(rotors[1])
        color = a == input ? MONOKAI_BLUE : MONOKAI_DARK
        render_trace!(ctx, rotors, a, color, render_params)
    end

    ctx
end
function render(rotors::Vector{Rotor};
    render_params::RotorRenderParams = RotorRenderParams(),
    )

    s, ctx = get_surface_and_context()

    rp2 = deepcopy(render_params)
    for (i,rotor) in enumerate(rotors)
        rp2.margin_left = get_vertex_x(render_params, i)
        render!(ctx, rotor, Int[], rp2)
    end

    s
end



let
    rotor = Rotor([4, 2, 1, 3])
    rotors = [rotor, rotor, rotor]
    @assert encode(rotors, 1) == 1
    @assert encode(rotors, 2) == 2
    @assert encode(rotors, 3) == 3
    @assert encode(rotors, 4) == 4
    @assert decode(rotors, 1) == 1
    @assert decode(rotors, 2) == 2
    @assert decode(rotors, 3) == 3
    @assert decode(rotors, 4) == 4
end

type Reflector
    perm::Vector{Int} # pairwise combinations, so perm[a] = b and perm[b] = a, a ≠ b
    function Reflector(perm::Vector{Int})
        for (i,p) in enumerate(perm)
            @assert perm[p] == i && i != p
        end
        new(perm)
    end
end
Base.length(ref::Reflector) = length(ref.perm)
encode(ref::Reflector, a::Int) = ref.perm[a]
decode(ref::Reflector, b::Int) = ref.perm[b]

let
    ref = Reflector([2,1,4,3])
    @assert encode(ref, 1) == 2
    @assert encode(ref, 2) == 1
    @assert encode(ref, 3) == 4
    @assert encode(ref, 4) == 3

    @assert decode(ref, 1) == 2
    @assert decode(ref, 2) == 1
    @assert decode(ref, 3) == 4
    @assert decode(ref, 4) == 3
end

type Plugboard
    perm::Vector{Int} # optional pairwise perms
end
Base.length(plugboard::Plugboard) = length(plugboard.perm)
encode(plugboard::Plugboard, a::Int) = plugboard.perm[a]
decode(plugboard::Plugboard, b::Int) = plugboard.perm[b]

type Enigma
    plugboard::Plugboard
    rotors::Vector{Rotor}
    ref::Reflector
    alphabet::Vector{Char}
    emulate_double_stepping::Bool # the original enigmas had a weird double-stepping effect that can also be emulated

    function Enigma(
        plugboard::Plugboard,
        rotors::Vector{Rotor},
        ref::Reflector,
        alphabet::Vector{Char},
        emulate_double_stepping::Bool=false,
        )
        new(plugboard, rotors, ref, alphabet, emulate_double_stepping)
    end
end
Base.length(enigma::Enigma) = length(enigma.ref)
function encode(enigma::Enigma, a::Int)
    b = encode(enigma.rotors, a)
    c = encode(enigma.ref, b)
    decode(enigma.rotors, c)
end
decode(enigma::Enigma, b::Int) = encode(enigma, b)

tick_with_mathematical_purity!(enigma::Enigma) = tick!(enigma.rotors)
function tick_with_double_stepping!(enigma::Enigma)

    # rotor 1 is always moved
    # if a rotor is in its notch position, it also moves the rotor to its right

    # 1 - identify rotors that are to be moved
    # 2 - move them

    n_rotors = length(enigma.rotors)
    move = falses(n_rotors)
    move[1] = true # always move the first rotor

    for (i,rotor) in enumerate(enigma.rotors)
        if i != n_rotors && mod(rotor.rotor_position,length(rotor))+1 in rotor.knock_points
            move[i] = true # rotate this guy
            move[i+1] = true # rotate next guy
        end
    end

    for (i,it_moves) in enumerate(move)
        if it_moves
            tick!(enigma.rotors[i])
        end
    end

    enigma
end
function tick!(enigma::Enigma)
    if enigma.emulate_double_stepping
        tick_with_double_stepping!(enigma)
    else
        tick_with_mathematical_purity!(enigma)
    end
    enigma
end

function encode!(enigma::Enigma, a::Int)
    #=
    Enigma Process
         1. Rotate wheels
         2. Encode
    =#

    tick!(enigma)
    b = encode(enigma, a)

    b
end

function render_trace!(
    ctx::CairoContext,
    enigma::Enigma,
    a::Int,
    color::Colorant,
    render_params::RotorRenderParams,
    render_plugboard::Bool,
    )

    P = get_vertex_pos(render_params, 1, a)
    render_vertex!(ctx, P, color, radius=render_params.vertex_radius)

    if render_plugboard
        b = encode(enigma.plugboard, a)
        Q = VecE2(P.x + render_params.sep_horz, get_vertex_y(render_params, b))
        render_bezier_edge!(ctx, P, Q, color, line_width=render_params.line_width)
        render_vertex!(ctx, Q, color, radius=render_params.vertex_radius)
        a, P = b, Q
    end

    for rotor in enigma.rotors
        b = encode(rotor, a)
        Q = VecE2(P.x + render_params.sep_horz, get_vertex_y(render_params, b))
        render_bezier_edge!(ctx, P, Q, color, line_width=render_params.line_width)
        render_vertex!(ctx, Q, color, radius=render_params.vertex_radius)
        a, P = b, Q
    end

    b = encode(enigma.ref, a)
    Q = VecE2(P.x, get_vertex_y(render_params, b))
    render_bezier_edge!(ctx, Q, P, color, reflect=true, line_width=render_params.line_width)
    render_vertex!(ctx, Q, color, radius=render_params.vertex_radius)
    a, P = b, Q

    for rotor in reverse(enigma.rotors)
        b = decode(rotor, a)
        Q = VecE2(P.x - render_params.sep_horz, get_vertex_y(render_params, b))
        render_bezier_edge!(ctx, Q, P, color, line_width=render_params.line_width)
        render_vertex!(ctx, Q, color, radius=render_params.vertex_radius)
        a, P = b, Q
    end

    if render_plugboard
        b = decode(enigma.plugboard, a)
        Q = VecE2(P.x - render_params.sep_horz, get_vertex_y(render_params, b))
        render_bezier_edge!(ctx, Q, P, color, line_width=render_params.line_width)
        render_vertex!(ctx, Q, color, radius=render_params.vertex_radius)
        a, P = b, Q
    end

    ctx
end
function render!(
    ctx::CairoContext,
    enigma::Enigma,
    input::Int,
    render_params::RotorRenderParams,
    render_plugboard::Bool,
    )

    rendered = Set{Int}()
    for a in 1 : length(enigma)
        if !in(a, rendered)
            b = encode(enigma, a)
            push!(rendered, b)
            color = a == input || b == input ? MONOKAI_BLUE : MONOKAI_DARK
            render_trace!(ctx, enigma, a, color, render_params, render_plugboard)
        end
    end

    ctx
end

function render_engima_tick!(
    ctx::CairoContext,
    enigma::Enigma,
    t::Float64, # ∈ [0,1]
    color::Colorant,
    render_params::RotorRenderParams = RotorRenderParams(),
    render_plugboard::Bool = true,
    )

    n = length(enigma)

    left_offset = 0

    if render_plugboard
        plugboard = enigma.plugboard
        for a in 1 : n
            b = encode(plugboard, a)
            A = get_vertex_pos(render_params, 1, a)
            B = get_vertex_pos(render_params, 2, b)
            render_bezier_edge!(ctx, A, B, color, line_width=render_params.line_width)
            render_vertex!(ctx, A, color, radius=render_params.vertex_radius)
            render_vertex!(ctx, B, color, radius=render_params.vertex_radius)
        end

        render_params.margin_left += render_params.sep_horz
    end

    n_rotors = length(enigma.rotors)
    rotor_index = 1
    done = false
    while !done
        rotor = enigma.rotors[rotor_index]

        rp2 = deepcopy(render_params)
        rp2.margin_left = render_params.margin_left + render_params.sep_horz*(rotor_index-1)

        render_rotor_tick!(ctx, rotor, t, color, rp2)
        done = rotor_index == n_rotors || !in(mod(rotor.rotor_position,n)+1, rotor.knock_points)
        rotor_index += 1
    end

    while rotor_index ≤ n_rotors

        rp2 = deepcopy(render_params)
        rp2.margin_left = render_params.margin_left + render_params.sep_horz*(rotor_index-1)

        render!(ctx, enigma.rotors[rotor_index], Int[], rp2)
        rotor_index += 1
    end

    # render the reflector
    rendered = Set{Int}()
    ref_x = get_vertex_x(render_params, n_rotors+1)
    for a in 1 : n
        if !in(a, rendered)
            b = encode(enigma.ref, a)
            push!(rendered, b)

            A = VecE2(ref_x, get_vertex_y(render_params, a))
            B = VecE2(ref_x, get_vertex_y(render_params, b))
            render_bezier_edge!(ctx, A, B, color, reflect=true, line_width=render_params.line_width)
            render_vertex!(ctx, A, color, radius=render_params.vertex_radius)
            render_vertex!(ctx, B, color, radius=render_params.vertex_radius)
        end
    end

    if render_plugboard
        render_params.margin_left -= render_params.sep_horz
    end

    ctx
end
function render_enigma_labels!(
    ctx::CairoContext,
    enigma::Enigma,
    render_params::RotorRenderParams = RotorRenderParams(),
    render_plugboard::Bool = true,
    font_size::Real=20.0,
    )

    P = VecE2(render_params.margin_left + render_params.sep_horz*0.5, get_vertex_y(render_params, 1) - 1.5*font_size)

    if render_plugboard
        render_text_centered(ctx, "Plugboard", P.x, P.y, font_size=font_size)
        P = P + VecE2(render_params.sep_horz, 0)
    end

    for i in 1 : length(enigma.rotors)
        text = @sprintf("Rotor %d", i)
        render_text_centered(ctx, text, P.x, P.y, font_size=font_size)
        P = P + VecE2(render_params.sep_horz, 0)
    end

    render_text_centered(ctx, "Reflector", P.x, P.y, font_size=font_size)

    ctx
end
function render_enigma_io!(
    ctx::CairoContext,
    enigma::Enigma,
    a::Int;
    render_params::RotorRenderParams = RotorRenderParams(),
    font_size::Real=20.0,
    only_input::Bool=false,
    )

    if 1 ≤ a ≤ length(enigma.alphabet)
        b = encode(enigma, a)

        x = get_vertex_x(render_params, 1) - 50.0
        P = VecE2(x, get_vertex_y(render_params, a))
        Q = VecE2(x, get_vertex_y(render_params, b))

        render_text_centered(ctx, string(enigma.alphabet[a]), P.x, P.y, font_size=font_size, color=MONOKAI_DARK)
        if !only_input
            render_text_centered(ctx, string(enigma.alphabet[b]), Q.x, Q.y, font_size=font_size, color=MONOKAI_COBALT)
        end
    end

    ctx
end
function render_text_and_ciphertext!(
    ctx::CairoContext,
    enigma::Enigma,
    text::String,
    cipher_text::String;
    render_params::RotorRenderParams = RotorRenderParams(),
    font_size::Real=20.0,
    only_input::Bool=false,
    font_face::String="monospace",
    )

    x = get_vertex_x(render_params, 1)
    y = get_vertex_y(render_params, length(enigma))

    render_text(ctx, text, x, y + 50, font_size=font_size, font_face=font_face, color=MONOKAI_DARK)
    if !only_input
        render_text(ctx, cipher_text, x, y + 100, font_size=font_size, font_face=font_face, color=MONOKAI_COBALT)
    end
    ctx
end
function render_text_and_ciphertext!(
    s::CairoSurface,
    enigma::Enigma,
    text::String,
    cipher_text::String;
    render_params::RotorRenderParams = RotorRenderParams(),
    font_size::Real=20.0,
    only_input::Bool=false,
    )

    ctx = creategc(s)
    render_text_and_ciphertext!(ctx, enigma, text, cipher_text, render_params=render_params, font_size=font_size, only_input=only_input)
    s
end

function render(enigma::Enigma, input::Int=-1;
    render_params::RotorRenderParams = RotorRenderParams(),
    render_plugboard::Bool=true,
    render_labels::Bool=true,
    render_input::Bool=true,
    do_not_render_trace::Bool=false,
    only_input::Bool=false,
    )

    s, ctx = get_surface_and_context()
    render!(ctx, enigma, do_not_render_trace ? -1 : input, render_params, render_plugboard)
    if render_labels
        render_enigma_labels!(ctx, enigma, render_params, render_plugboard)
    end
    if render_input
        render_enigma_io!(ctx, enigma, input, render_params=render_params, only_input=only_input)
    end
    s
end

function play_animation(rotor::Rotor, input::String, alphabet::Vector{Char};
    render_params::RotorRenderParams = RotorRenderParams(),
    font_size::Real=20.0,
    fps::Int=12,
    fp_render_nothing=6,
    fp_show_input_first=4,
    fp_show_trace=8,
    )

    input_text = ""
    cipher_text = ""

    frames = Frames(MIME("image/png"), fps=fps)

    # render empty machine first
    s, ctx = get_surface_and_context()
    render!(ctx, rotor, Int[], render_params)
    render_alphabet!(ctx, rotor, alphabet, render_params=render_params, font_size=font_size)
    render_text_and_ciphertext!(s, rotor, input_text, cipher_text, render_params=render_params, font_size=1.5*font_size, only_input=false)
    for i in 1 : fp_render_nothing
        push!(frames, s)
    end

    input_index = 1
    while input_index ≤ length(input)

        while input_index ≤ length(input) && input[input_index] == ' '
            input_text = input_text * " "
            cipher_text = cipher_text * " "
            input_index += 1
        end
        if input_index > length(input)
            break
        end

        A = input[input_index]
        a = findfirst(alphabet, A)

        # show input first
        s, ctx = get_surface_and_context()
        render!(ctx, rotor, Int[], render_params)
        render_alphabet!(ctx, rotor, alphabet, render_params=render_params, font_size=font_size)
        render_text_and_ciphertext!(s, rotor, input_text, cipher_text, render_params=render_params, font_size=1.5*font_size, only_input=false)
        for i in 1 : fp_show_input_first
            push!(frames, s)
        end

        # show trace
        b = encode(rotor, a)
        B = alphabet[b]
        input_text = input_text * string(A)
        cipher_text = cipher_text * string(B)
        s, ctx = get_surface_and_context()
        render!(ctx, rotor, [a], render_params)
        render_alphabet!(ctx, rotor, alphabet, render_params=render_params, font_size=font_size)
        render_text_and_ciphertext!(s, rotor, input_text, cipher_text, render_params=render_params, font_size=1.5*font_size, only_input=false)
        for i in 1 : fp_show_trace
            push!(frames, s)
        end

        input_index += 1
    end

    frames
end
function play_animation(rotors::Vector{Rotor}, input::String, alphabet::Vector{Char};
    render_params::RotorRenderParams = RotorRenderParams(),
    font_size::Real=20.0,
    fps::Int=12,
    fp_render_nothing=6,
    fp_show_input_first=2,
    fp_rot::Int=24,
    fp_show_trace=4,
    )

    input_text = ""
    cipher_text = ""

    frames = Frames(MIME("image/png"), fps=fps)

    # render empty machine first
    s = render(rotors, render_params=render_params)
    ctx = creategc(s)
    render_text_and_ciphertext!(ctx, rotors[1], input_text, cipher_text, render_params=render_params, font_size=1.5*font_size)
    render_alphabet!(ctx, rotors, alphabet, render_params=render_params, font_size=font_size)
    for i in 1 : fp_render_nothing
        push!(frames, s)
    end

    input_index = 1
    while input_index ≤ length(input)

        while input_index ≤ length(input) && input[input_index] == ' '
            input_text = input_text * " "
            cipher_text = cipher_text * " "
            input_index += 1
        end
        if input_index > length(input)
            break
        end

        A = input[input_index]
        a = findfirst(alphabet, A)

        # rotate the machine
        for i in 1 : fp_rot
            t = i/fp_rot
            s, ctx = get_surface_and_context()
            render_rotor_tick!(ctx, rotors, t, MONOKAI_DARK, render_params)
            render_text_and_ciphertext!(ctx, rotors[1], input_text, cipher_text, render_params=render_params, font_size=1.5*font_size)
            render_alphabet!(ctx, rotors, alphabet, render_params=render_params, font_size=font_size)
            push!(frames, s)
        end

        # show trace
        tick!(rotors)
        b = encode(rotors, a)
        B = alphabet[b]
        input_text = input_text * string(A)
        cipher_text = cipher_text * string(B)
        s, ctx = get_surface_and_context()
        render!(ctx, rotors, a, render_params)
        render_text_and_ciphertext!(ctx, rotors[1], input_text, cipher_text, render_params=render_params, font_size=1.5*font_size)
        render_alphabet!(ctx, rotors, alphabet, render_params=render_params, font_size=font_size)
        for i in 1 : fp_show_trace
            push!(frames, s)
        end

        input_index += 1
    end

    frames
end
function play_animation(enigma::Enigma, input::String;
    render_params::RotorRenderParams = RotorRenderParams(),
    render_plugboard::Bool=true,
    font_size::Real=20.0,
    fps::Int=12,
    fp_render_nothing=6,
    fp_show_input_first=2,
    fp_rot::Int=24,
    fp_show_trace=4,
    )

    input_text = ""
    cipher_text = ""

    frames = Frames(MIME("image/png"), fps=fps)

    # render empty machine first
    s = render(enigma, render_params=render_params, render_plugboard=render_plugboard)
    s = render_text_and_ciphertext!(s, enigma, input_text, cipher_text, render_params=render_params, font_size=1.5*font_size, only_input=false)
    for i in 1 : fp_render_nothing
        push!(frames, s)
    end

    input_index = 1
    while input_index ≤ length(input)

        while input_index ≤ length(input) && input[input_index] == ' '
            input_text = input_text * " "
            cipher_text = cipher_text * " "
            input_index += 1
        end
        if input_index > length(input)
            break
        end

        A = input[input_index]
        a = findfirst(enigma.alphabet, A)

        # show input first
        s = render(enigma, a, render_params=render_params, render_plugboard=render_plugboard, do_not_render_trace=true, only_input=true)
        s = render_text_and_ciphertext!(s, enigma, input_text, cipher_text, render_params=render_params, font_size=1.5*font_size, only_input=false)
        for i in 1 : fp_show_input_first
            push!(frames, s)
        end

        # rotate the machine
        for i in 1 : fp_rot
            t = i/fp_rot
            s, ctx = get_surface_and_context()
            render_engima_tick!(ctx, enigma, t, MONOKAI_DARK, render_params, render_plugboard)
            render_enigma_labels!(ctx, enigma, render_params, render_plugboard)
            render_enigma_io!(ctx, enigma, a, render_params=render_params, font_size=font_size, only_input=true)
            render_text_and_ciphertext!(ctx, enigma, input_text, cipher_text, render_params=render_params, font_size=1.5*font_size, only_input=false)
            push!(frames, s)
        end

        # show trace
        tick!(enigma)
        b = encode(enigma, a)
        B = enigma.alphabet[b]
        input_text = input_text * string(A)
        cipher_text = cipher_text * string(B)
        s = render(enigma, a, render_params=render_params, render_plugboard=render_plugboard, do_not_render_trace=false, only_input=false)
        s = render_text_and_ciphertext!(s, enigma, input_text, cipher_text, render_params=render_params, font_size=1.5*font_size, only_input=false)
        for i in 1 : fp_show_trace
            push!(frames, s)
        end

        input_index += 1
    end

    frames
end

include("fullsize.jl")