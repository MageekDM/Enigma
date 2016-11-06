using Cairo
using Colors
using Vec

const MONOKAI_LITE = colorant"0xCFBFADFF"
const MONOKAI_DARK = colorant"0x272822FF"
const MONOKAI_BLUE   = colorant"0x52E3F6FF"
const MONOKAI_GREEN  = colorant"0xA7EC21FF"
const MONOKAI_RED    = colorant"0xFF007FFF"
const MONOKAI_ORANGE = colorant"0xF9971FFF"
const MONOKAI_COBALT = colorant"0x79ABFFFF"

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

function get_surface_and_context(canvas_width::Int, canvas_height::Int)
    s = CairoRGBSurface(canvas_width, canvas_height)
    ctx = creategc(s)
    (s, ctx)
end
function get_surface_and_context(;
    canvas_width::Int = 1000,
    canvas_height::Int = 400,
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
function render!(ctx::CairoContext,
    rotor::Rotor,
    active_inputs::Vector{Int},
    margin_left::Real,
    margin_top::Real,
    sep_vert::Real,
    sep_horz::Real,
    )

    # compute the vertices
    n = length(rotor.enshift)
    points_left = Array(VecE2, n)
    points_right = Array(VecE2, n)
    active_left = falses(n)
    active_right = falses(n)
    for i in 1 : n
        points_left[i] = VecE2(margin_left, margin_top + sep_vert*(i-1))
        j = encode(rotor, i)
        points_right[i] = VecE2(margin_left+sep_horz, margin_top + sep_vert*(j-1))

        if i in active_inputs
            active_left[i] = true
            active_right[i] = true
        end
    end

    # render the vertices
    radius = 10.0
    save(ctx)
    for (points, active) in zip((points_left, points_right),
                                (active_left, active_right))
        for (i, pt) in enumerate(points)
            render_vertex!(ctx, pt, active_left[i] ? MONOKAI_COBALT : MONOKAI_DARK)
        end
    end
    restore(ctx)

    # render the connections
    save(ctx)
    for i in 1 : n
        A, D = points_left[i], points_right[i]
        render_bezier_edge!(ctx, A, D, active_left[i] ? MONOKAI_COBALT : MONOKAI_DARK)
    end
    restore(ctx)

    ctx
end
function render(rotor::Rotor;
    active_inputs::Vector{Int}=Int[],
    margin_left::Float64 = 200.0,
    margin_top::Float64 = 50.0,
    sep_vert::Float64 = 100.0,
    sep_horz::Float64 = 200.0,
    )

    s, ctx = get_surface_and_context()
    render!(ctx, rotor, active_inputs, margin_left, margin_top, sep_vert, sep_horz)
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
function render(rotors::Vector{Rotor};
    margin_top::Float64 = 50.0,
    sep_vert::Float64 = 100.0,
    sep_horz::Float64 = 200.0,
    )

    s, ctx = get_surface_and_context()

    for (i,rotor) in enumerate(rotors)
        margin_left = 200.0*i
        render!(ctx, rotor, Int[], margin_left, margin_top, sep_vert, sep_horz)
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
end
Base.length(enigma::Enigma) = length(enigma.ref)
function encode(enigma::Enigma, a::Int)
    b = encode(enigma.rotors, a)
    c = encode(enigma.ref, b)
    decode(enigma.rotors, c)
end
decode(enigma::Enigma, b::Int) = encode(enigma, b)
function tick!(enigma::Enigma, rotor_index::Int=1)
    rotor = enigma.rotors[rotor_index]

    if mod(rotor.rotor_position,length(rotor))+1 in rotor.knock_points && rotor_index < length(enigma.rotors)
        # rotate the next rotor as well
        tick!(enigma, rotor_index+1)
    end
    tick!(rotor) # rotate this rotor
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
    margin_left::Real,
    margin_top::Real,
    sep_vert::Real,
    sep_horz::Real,
    render_plugboard::Bool,
    )

    P = VecE2(margin_left, margin_top + sep_vert*(a-1))
    render_vertex!(ctx, P, color)

    if render_plugboard
        b = encode(enigma.plugboard, a)
        Q = VecE2(P.x + sep_horz, margin_top + sep_vert*(b-1))
        render_bezier_edge!(ctx, P, Q, color)
        render_vertex!(ctx, Q, color)
        a, P = b, Q
    end

    for rotor in enigma.rotors
        b = encode(rotor, a)
        Q = VecE2(P.x + sep_horz, margin_top + sep_vert*(b-1))
        render_bezier_edge!(ctx, P, Q, color)
        render_vertex!(ctx, Q, color)
        a, P = b, Q
    end

    b = encode(enigma.ref, a)
    Q = VecE2(P.x, margin_top + sep_vert*(b-1))
    render_bezier_edge!(ctx, Q, P, color, reflect=true)
    render_vertex!(ctx, Q, color)
    a, P = b, Q

    for rotor in reverse(enigma.rotors)
        b = decode(rotor, a)
        Q = VecE2(P.x - sep_horz, margin_top + sep_vert*(b-1))
        render_bezier_edge!(ctx, Q, P, color)
        render_vertex!(ctx, Q, color)
        a, P = b, Q
    end

    if render_plugboard
        b = decode(enigma.plugboard, a)
        Q = VecE2(P.x - sep_horz, margin_top + sep_vert*(b-1))
        render_bezier_edge!(ctx, P, Q, color, reflect=true)
        render_vertex!(ctx, Q, color)
        a, P = b, Q
    end

    ctx
end
function render!(
    ctx::CairoContext,
    enigma::Enigma,
    input::Int,
    margin_left::Real,
    margin_top::Real,
    sep_vert::Real,
    sep_horz::Real,
    render_plugboard::Bool,
    )

    rendered = Set{Int}()
    for a in 1 : length(enigma)
        if !in(a, rendered)
            b = encode(enigma, a)
            push!(rendered, b)
            color = a == input || b == input ? MONOKAI_BLUE : MONOKAI_DARK
            render_trace!(ctx, enigma, a, color, margin_left, margin_top, sep_vert, sep_horz, render_plugboard)
        end
    end

    ctx
end
function render(enigma::Enigma, input::Int=-1;
    margin_left::Float64 = 200.0,
    margin_top::Float64 = 50.0,
    sep_vert::Float64 = 100.0,
    sep_horz::Float64 = 200.0,
    render_plugboard::Bool=true,
    )

    s, ctx = get_surface_and_context()
    render!(ctx, enigma, input, margin_left, margin_top, sep_vert, sep_horz, render_plugboard)
    s
end

function render_rotor_tick!(
    ctx::CairoContext,
    rotor::Rotor,
    t::Float64, # ∈ [0,1]
    color::Colorant,
    margin_left::Real,
    margin_top::Real,
    sep_vert::Real,
    sep_horz::Real,
    )

    n = length(rotor)
    for a1 in 1 : n

        a2 = mod(a1, n)+1
        P1 = VecE2(margin_left, margin_top + sep_vert*(a1-1))
        P2 = VecE2(margin_left, margin_top + sep_vert*(a2-1))

        if a2 != 1
            P = bezier(P1, P2, t)
        else # use a special bezier
            C = P1 + VecE2(0, 100.0)
            D = P2 - VecE2(0, 100.0)
            P = bezier(P1, C, D, P2, t)
        end

        b1 = encode(rotor, a1)
        b2 = mod(b1,n)+1
        Q1 = VecE2(P1.x + sep_horz, margin_top + sep_vert*(b1-1))
        Q2 = VecE2(P1.x + sep_horz, margin_top + sep_vert*(b2-1))

        if b2 != 1
            Q = bezier(Q1, Q2, t)
        else
            C = Q1 + VecE2(0, 100.0)
            D = Q2 - VecE2(0, 100.0)
            Q = bezier(Q1, C, D, Q2, t)
        end

        render_vertex!(ctx, P, color)
        render_vertex!(ctx, Q, color)
        render_bezier_edge!(ctx, P, Q, color)
    end

    ctx
end
function render_engima_tick!(
    ctx::CairoContext,
    enigma::Enigma,
    t::Float64, # ∈ [0,1]
    color::Colorant,
    margin_left::Real,
    margin_top::Real,
    sep_vert::Real,
    sep_horz::Real,
    render_plugboard::Bool,
    )

    n = length(enigma)

    if render_plugboard
        plugboard = enigma.plugboard
        for a in 1 : n
            b = encode(plugboard, a)
            A = VecE2(margin_left, margin_top + sep_vert*(a-1))
            B = VecE2(margin_left + sep_horz, margin_top + sep_vert*(b-1))
            render_bezier_edge!(ctx, A, B, color)
            render_vertex!(ctx, A, color)
            render_vertex!(ctx, B, color)
        end

        margin_left += sep_horz
    end

    n_rotors = length(enigma.rotors)
    rotor_index = 1
    done = false
    while !done
        rotor = enigma.rotors[rotor_index]
        render_rotor_tick!(ctx, rotor, t, color, margin_left+sep_horz*(rotor_index-1), margin_top, sep_vert, sep_horz)
        done = rotor_index != n_rotors || in(mod(rotor.rotor_position,n)+1, rotor.knock_points)
        rotor_index += 1
    end

    while rotor_index ≤ n_rotors
        render!(ctx, rotor, Int[], margin_left+sep_horz*(rotor_index-1), margin_top, sep_vert, sep_horz)
        rotor_index += 1
    end

    # render the reflector
    rendered = Set{Int}()
    ref_x = margin_left+sep_horz*n_rotors
    for a in 1 : n
        if !in(a, rendered)
            b = encode(enigma.ref, a)
            push!(rendered, b)

            A = VecE2(ref_x, margin_top + sep_vert*(a-1))
            B = VecE2(ref_x, margin_top + sep_vert*(b-1))
            render_bezier_edge!(ctx, A, B, color, reflect=true)
            render_vertex!(ctx, A, color)
            render_vertex!(ctx, B, color)
        end
    end

    ctx
end

include("fullsize.jl")