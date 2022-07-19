using Printf

@enum Cell unknown on off

const Runs = AbstractVector{I} where {I <: Integer}
const RunList = AbstractVector{I} where {II <: Integer, I <: AbstractVector{II}}
const Line = AbstractVector{C} where {C <: Cell}

struct Board
    row_runs::RunList
    col_runs::RunList
    cells::Matrix{Cell}
end

function Board(row_runs::AbstractArray, col_runs::AbstractArray)
    length(row_runs) != length(col_runs) &&
        error("Number of row runs != number of col runs")
    Board(
        convert(Vector{Vector{Int}}, row_runs),
        convert(Vector{Vector{Int}}, col_runs),
        fill(unknown, length(row_runs), length(row_runs))
    )
end

Base.size(b::Board) = size(b.cells, 1)

Base.view(b::Board, dim1, dim2) = view(b.cells, dim1, dim2)

function holes(line::Line)
    result = []
    prev = off
    start = 0
    for (i, c) in enumerate(line)
        prev == off && c != off && (start = i)
        prev != off && c == off && push!(result, @view line[start:i-1])
        prev = c
    end
    if prev != off
        push!(result, @view line[start:end])
    end
    result
end

function line_solve(board::Board; diag = false)
    changed = true
    while changed
        changed = false
        for row = 1:size(board)
            localchanged = solve(view(board, row, :), board.row_runs[row]; diag)
            localchanged && println(board)
            changed |= localchanged
        end
        for col = 1:size(board)
            localchanged = solve(view(board, : ,col), board.col_runs[col]; diag)
            localchanged && println(board)
            changed |= localchanged
        end
    end
end

function solve(line::Line, runs::Runs; diag = false)
    rev_line = @view line[end:-1:1]
    rev_runs = @view runs[end:-1:1]
    isempty(valid(runs)) && return false
    solve_forward(line, valid(runs), diag) |
        result(mark_complete_forward, line, valid(runs), diag) |
        #result(fill_middle, line, valid(runs), diag) |
        solve_forward(rev_line, valid(rev_runs), diag) |
        result(mark_complete_forward, rev_line, valid(rev_runs), diag)
end

function result(func, line, runs, diag)
    diag && (oldline = [line...])
    result = func(line, runs)
    diag && result && oldline != line && println("$func:\n $oldline\n ->\n $line")
    result
end

function set(hole, value)
    all(==(value), hole) && return false
    replace!(x-> value, hole)
    true
end

function solve_forward(line::Line, runs::Runs, diag)
    isempty(runs) && return false
    changed = false
    first = runs[1]
    changed |= result(empty_leading_small, line, first, diag)
    changed |= result(fill_first, line, runs, diag)
    changed |= result(empty_unreachable, line, runs, diag)
    changed
end

function empty_leading_small(line::Line, first)
    changed = false
    for hole in holes(line)
        length(hole) >= first && break
        changed |= set(hole, off)
    end
    changed
end

"""
The first hole should be large enough for the first run
Remove trailing runs that can fit into trailing holes
Fit the first into the hole, based on the remaining runs
"""
function fill_first(line::Line, runs::Runs)
    changed = false
    allholes = holes(line)
    while !isempty(runs) && !isempty(allholes)
        curhole = allholes[1]
        allholes = @view allholes[2:end]
        first = runs[1]
        if length(curhole) < first
            changed |= set(curhole, off)
            continue
        end
        tmp_runs = trim_trailing(allholes, runs[2:end])
        runs = @view runs[2 + length(tmp_runs) : end]
        hole = @view curhole[1 : end - length(tmp_runs) - sum(tmp_runs)]
        run_len = 2 * first - length(hole)
        if run_len > 0
            start = (length(hole) - run_len) ÷ 2 + 1
            finish = start + run_len - 1
            finish < start && continue
            changed |= set(@view(hole[start : finish]), on)
        end
    end
    changed
end

function empty_unreachable(line::Line, runs::Runs)
    changed = false
    allholes = holes(line)
    while !isempty(runs) && !isempty(allholes)
        curhole = allholes[1]
        allholes = @view allholes[2:end]
        first = runs[1]
        if length(curhole) < first
            changed |= set(curhole, off)
            continue
        end
        tmp_runs = trim_trailing(allholes, runs[2:end])
        runs = @view runs[2 + length(tmp_runs) : end]
        !isempty(tmp_runs) && continue
        start = findfirst(==(on), curhole)
        start === nothing && continue
        finish = findlast(==(on), curhole)
        earliest = finish - first
        latest = start + first
        changed |= earliest >= 1 && set(@view(curhole[1:earliest]), off)
        changed |= latest <= length(curhole) && set(@view(curhole[latest:end]), off)
    end
    changed
end

"""
Eliminate trailing runs that can be the only ones to fit
in trailing holes
"""
function trim_trailing(holes::AbstractVector, runs::Runs)
    isempty(holes) && return runs
    result = @view runs[1:end]
    holes = @view holes[1:end]
    while !isempty(holes)
        last = holes[end]
        runlen = 0
        for run in @view result[1:end]
            newlen = run + (runlen > 0 ? runlen + 1 : 0)
            newlen > length(last) && break
            result = @view result[1:end - 1]
        end
        holes = @view holes[1:end - 1]
    end
    result
end

function mark_complete_forward(line::Line, runs::Runs)
    allholes = holes(line)
    changed = false
    for i in 1:min(length(allholes), length(runs))
        hole = allholes[i]
        run = runs[i]
        (hole[1] != on || length(hole) < run) && break
        set(@view(hole[1 : run]), on)
        if length(hole) > run
            hole[run + 1] != off
            hole[run + 1] = off
        end
        runs[i] = -runs[i] # mark the run as complete
        changed = true
    end
    changed
end

"""
return a view of runs that haven't been completed yet
(i.e. scrape out the negative ones)
"""
function valid(runs::Runs)
    indices = [i for (i, r) in enumerate(runs) if r > 0]
    length(indices) == length(runs) ? runs : view(runs, indices)
end

#
# Diagnostics
#

line(s::AbstractString) = map(cell, collect(s))
line(size::Integer) = fill(unknown, size)

macro line_str(s) :(line($s)) end

cell(c::Char) = c == '.' ? unknown : c == '@' ? on : off
char(c::Cell) = c == on ? '@' : c == off ? 'X' : '.'

Base.show(io::IO, cell::Cell) = print(io, char(cell))
Base.show(io::IO, line::Line) = for c in line
    show(io, c)
end
function Base.show(io::IO, board::Board)
    row_runs = map(r-> abs.(r), board.row_runs)
    col_runs = map(r-> abs.(r), board.col_runs)
    row_run_wids = []
    for runs in row_runs
        wid = max(0, filter(!isempty, runs)...) > 9 ? length(runs) : 0
        for run in runs
            wid += run > 9 ? 2 : 1
        end
        push!(row_run_wids, wid)
    end
    row_pad = max(row_run_wids...)
    max_row_run = max(max.([0], filter(!isempty, row_runs)...)...)
    row_run_sep = max_row_run > 9 ? " " : ""
    max_col_runs = max(length.(col_runs)...)
    col_wids = max(max.([0], filter(!isempty, col_runs)...)...) > 9 ? 2 : 1
    for r in max_col_runs:-1:1
        Printf.format(stdout, Printf.Format("%$(row_pad)s"), " ")
        for c in col_runs
            Printf.format(io, Printf.Format("%-$(col_wids)s"),
                          r <= length(c) ? c[r] : " ")
        end
        println()
    end
    for row in 1:size(board)
        Printf.format(io, Printf.Format("%$(row_pad)s"), join(row_runs[row], row_run_sep))
        show(io, view(board.cells, row, :))
        row < size(board) && println(io)
    end
end

function solve(rows::AbstractArray, cols::AbstractArray; diag = false)
    b = Board(rows, cols)
    println("STARTING BOARD:")
    println(b)
    line_solve(b; diag)
    println(any(==(unknown), b.cells) ? "UN" : "", "SOLVED BOARD:")
    println(b)
    println()
end

#
# Tests
#

solve([[1, 1], [1, 1], [1, 1]], [[3], [], [3]])

solve([[1, 1], [1, 2], [2], [3], [3]], [[2, 2], [3], [3], [1], [2]]; diag = true)

solve([[],[1], [1], [], []], [[2], [1, 1], [1, 1], [1, 1], [2, 1]]; diag = true)

nothing
