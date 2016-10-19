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

type Rotor
    perm::Vector{Int}
    rotor_position::Int
    Rotor(perm::Vector{Int}, rotor_position::Int=0) = new(perm, rotor_position)
end
function encode(rotor::Rotor, a::Int)
    n = length(rotor.perm) # number of letters
    a₂ = a - rotor.rotor_position
    b = rotor.perm[mod(a₂-1, n)+1]
    Δ = b - a₂
    mod(a + Δ - 1, n)+1
end
function decode(rotor::Rotor, a::Int)
    for b in 1 : length(rotor.perm)
        if encode(rotor, b) == a
            return b
        end
    end
    error("decode not possible")
end

function render(rotor::Rotor;
    active_inputs::Vector{Int}=Int[],
    )

    canvas_width, canvas_height = 1000, 400
    s, ctx = get_surface_and_context(canvas_width, canvas_height)

    # fill with background color
    set_source_rgba( ctx, colorant"white")
    paint(ctx)

    # compute the vertices
    margin_left = 200.0
    margin_top = 50.0
    sep_vert = 100.0
    sep_horz = 200.0

    n = length(rotor.perm)
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
            save(ctx)
            set_source_rgba(ctx, active[i] ? MONOKAI_COBALT : MONOKAI_DARK)
            translate(ctx, pt.x, pt.y)
            arc(ctx, 0.0, 0.0, radius, 0, 2pi)
            fill(ctx)
            restore(ctx)
        end
    end
    restore(ctx)

    # render the connections
    line_width = 5.0
    bezier_strength = 150.0
    save(ctx)
    set_line_width(ctx, line_width)
    set_source_rgba(ctx, MONOKAI_DARK)
    for i in 1 : n
        A, D = points_left[i], points_right[i]

        B = A + VecE2(bezier_strength, 0.0)
        C = D - VecE2(bezier_strength, 0.0)

        save(ctx)
        set_source_rgba(ctx, active_left[i] ? MONOKAI_COBALT : MONOKAI_DARK)
        move_to(ctx, A.x, A.y)
        curve_to(ctx, B.x, B.y, C.x, C.y, D.x, D.y)
        stroke(ctx)
        restore(ctx)
    end
    restore(ctx)

    s
end