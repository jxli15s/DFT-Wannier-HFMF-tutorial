!#####################################################################
! Module : gnu
! Content: gnuplot script related
! Author : Q. X. & J. X. Li
! Date   : 2017-04-21
!#####################################################################
module gnu

contains

!
!     %---------------------------------------------%
!       write gnu terminal to a gnuplot script iu
!     %---------------------------------------------%
!
subroutine write_gnu_terminal(iu,s,fontsize)
integer, intent(in) :: iu
character(*), intent(in) :: s
integer, intent(in), optional :: fontsize
integer :: fs

if (.not. present(fontsize)) then
fs = 10
else
fs = fontsize
end if

write(iu,"(a)")
write(iu,"(a,i2,a)") &
&    "#set terminal X11 enhanced font 'Helvetica,", fs, "' persist"
write(iu,"(a)")
write(iu,"(a,i2,a)") &
&   "set terminal qt enhanced font 'Helvetica,", fs, "' persist"
write(iu,"(a)")
write(iu,"(a,i2,a)") &
&   "###set terminal postscript eps enhanced color 'Times-Roman,", fs, "'"
write(iu,"(a,i2,a)") &
&   "#set terminal postscript eps enhanced color 'Helvetica,", fs, "'"
write(iu,"(a)") "#set output '"//trim(adjustl(s))//".eps'"
write(iu,"(a)") "#print 'The output is "//trim(adjustl(s))//".eps'"
write(iu,"(a)")
write(iu,"(a)") "#set terminal epslatex standalone color"
write(iu,"(a)") "#set output '"//trim(adjustl(s))//".tex'"
write(iu,"(a)") "#print 'The output is "//trim(adjustl(s))//".tex'"
write(iu,"(a)")
end subroutine

!
!     %---------------------------------------------%
!         write line style to a gnuplot script iu
!     %---------------------------------------------%
!
subroutine write_gnu_linestyle( iu, m )
integer, intent(in) :: iu
integer, intent(in), optional :: m
integer :: i, n, info

character(len=128) :: s

call write_linestyle()

if (.not. present(m)) then
n = 4
else
n = m
end if

open(unit=99, file="linestyle.gnu", status="old")
do i = 1, n
read(unit=99,fmt="(a128)",iostat=info) s
if (info /= 0) then
s = " "
rewind(99)
end if 
write(iu,"(a)") trim(s)
enddo
close(99)

call delete_linestyle()

end subroutine

!
!     %---------------------------%
!       write linestyle.gnu file
!     %---------------------------%
!
subroutine write_linestyle( )
open(unit=99, file="linestyle.gnu", status="replace")
write(99,"(a)") "set style line 1   lc rgb '#ffffff' lt 1 lw 1 # white"
write(99,"(a)") "set style line 2   lc rgb '#000000' lt 1 lw 1 # black"

write(99,"(a)") "set style line 47  lc rgb '#666666' lt 1 lw 1 # grey40"

write(99,"(a)") "set style line 3   lc rgb '#dd181f' lt 1 lw 1 # red"
write(99,"(a)") "set style line 4   lc rgb '#0060ad' lt 1 lw 1 # blue"
write(99,"(a)") "set style line 5   lc rgb '#006400' lt 1 lw 1 # dark-green"
write(99,"(a)") "set style line 6   lc rgb '#b8860b' lt 1 lw 1 # dark-goldenrod"
write(99,"(a)") "set style line 7   lc rgb '#c000ff' lt 1 lw 1 # dark-magenta"
write(99,"(a)") "set style line 8   lc rgb '#00eeee' lt 1 lw 1 # dark-cyan"
write(99,"(a)") "set style line 9   lc rgb '#c04000' lt 1 lw 1 # dark-orange"
write(99,"(a)") "set style line 10  lc rgb '#c8c800' lt 1 lw 1 # dark-yellow"
write(99,"(a)") "set style line 11  lc rgb '#4169e1' lt 1 lw 1 # royalblue"
write(99,"(a)") "set style line 12  lc rgb '#ffc020' lt 1 lw 1 # goldenrod"
write(99,"(a)") "set style line 13  lc rgb '#008040' lt 1 lw 1 # dark-spring-green"
write(99,"(a)") "set style line 14  lc rgb '#c080ff' lt 1 lw 1 # purple"
write(99,"(a)") "set style line 15  lc rgb '#306080' lt 1 lw 1 # steelblue"
write(99,"(a)") "set style line 16  lc rgb '#8b0000' lt 1 lw 1 # dark-red"
write(99,"(a)") "set style line 17  lc rgb '#408000' lt 1 lw 1 # dark-chartreuse"
write(99,"(a)") "set style line 18  lc rgb '#ff80ff' lt 1 lw 1 # orchid"
write(99,"(a)") "set style line 19  lc rgb '#7fffd4' lt 1 lw 1 # aquamarine"
write(99,"(a)") "set style line 20  lc rgb '#a52a2a' lt 1 lw 1 # brown"
write(99,"(a)") "set style line 21  lc rgb '#ffb6c1' lt 1 lw 1 # light-pink"
write(99,"(a)") "set style line 22  lc rgb '#afeeee' lt 1 lw 1 # light-turquoise"
write(99,"(a)") "set style line 23  lc rgb '#ffd700' lt 1 lw 1 # gold"
write(99,"(a)") "set style line 24  lc rgb '#00ff00' lt 1 lw 1 # green"
write(99,"(a)") "set style line 25  lc rgb '#006400' lt 1 lw 1 # dark-green"
write(99,"(a)") "set style line 26  lc rgb '#00ff7f' lt 1 lw 1 # spring-green"
write(99,"(a)") "set style line 27  lc rgb '#228b22' lt 1 lw 1 # forest-green"
write(99,"(a)") "set style line 28  lc rgb '#2e8b57' lt 1 lw 1 # sea-green"
write(99,"(a)") "set style line 29  lc rgb '#0000ff' lt 1 lw 1 # blue"
write(99,"(a)") "set style line 30  lc rgb '#00008b' lt 1 lw 1 # dark-blue"
write(99,"(a)") "set style line 31  lc rgb '#191970' lt 1 lw 1 # midnight-blue"
write(99,"(a)") "set style line 32  lc rgb '#000080' lt 1 lw 1 # navy"
write(99,"(a)") "set style line 33  lc rgb '#0000cd' lt 1 lw 1 # medium-blue"
write(99,"(a)") "set style line 34  lc rgb '#87ceeb' lt 1 lw 1 # skyblue"
write(99,"(a)") "set style line 35  lc rgb '#00ffff' lt 1 lw 1 # cyan"
write(99,"(a)") "set style line 36  lc rgb '#ff00ff' lt 1 lw 1 # magenta"
write(99,"(a)") "set style line 37  lc rgb '#00ced1' lt 1 lw 1 # dark-turquoise"
write(99,"(a)") "set style line 38  lc rgb '#ff1493' lt 1 lw 1 # dark-pink"
write(99,"(a)") "set style line 39  lc rgb '#ff7f50' lt 1 lw 1 # coral"
write(99,"(a)") "set style line 40  lc rgb '#f08080' lt 1 lw 1 # light-coral"
write(99,"(a)") "set style line 41  lc rgb '#ffff00' lt 1 lw 1 # yellow"
write(99,"(a)") "set style line 42  lc rgb '#40e0d0' lt 1 lw 1 # turquoise"
write(99,"(a)") "set style line 43  lc rgb '#000000' lt 1 lw 1 # grey0"
write(99,"(a)") "set style line 44  lc rgb '#1a1a1a' lt 1 lw 1 # grey99"
write(99,"(a)") "set style line 45  lc rgb '#333333' lt 1 lw 1 # grey20"
write(99,"(a)") "set style line 46  lc rgb '#4d4d4d' lt 1 lw 1 # grey30"

write(99,"(a)") "set style line 48  lc rgb '#7f7f7f' lt 1 lw 1 # grey50"
write(99,"(a)") "set style line 49  lc rgb '#999999' lt 1 lw 1 # grey60"
write(99,"(a)") "set style line 50  lc rgb '#b3b3b3' lt 1 lw 1 # grey70"
write(99,"(a)") "set style line 51  lc rgb '#c0c0c0' lt 1 lw 1 # grey"
write(99,"(a)") "set style line 52  lc rgb '#cccccc' lt 1 lw 1 # grey80"
write(99,"(a)") "set style line 53  lc rgb '#e5e5e5' lt 1 lw 1 # grey90"
write(99,"(a)") "set style line 54  lc rgb '#ffffff' lt 1 lw 1 # grey990"
write(99,"(a)") "set style line 55  lc rgb '#f03232' lt 1 lw 1 # light-red"
write(99,"(a)") "set style line 56  lc rgb '#90ee90' lt 1 lw 1 # light-green"
write(99,"(a)") "set style line 57  lc rgb '#add8e6' lt 1 lw 1 # light-blue"
write(99,"(a)") "set style line 58  lc rgb '#f055f0' lt 1 lw 1 # light-magenta"
write(99,"(a)") "set style line 59  lc rgb '#e0ffff' lt 1 lw 1 # light-cyan"
write(99,"(a)") "set style line 60  lc rgb '#eedd82' lt 1 lw 1 # light-goldenrod"
write(99,"(a)") "set style line 61  lc rgb '#ff4500' lt 1 lw 1 # orange-red"
write(99,"(a)") "set style line 62  lc rgb '#fa8072' lt 1 lw 1 # salmon"
write(99,"(a)") "set style line 63  lc rgb '#e9967a' lt 1 lw 1 # dark-salmon"
write(99,"(a)") "set style line 64  lc rgb '#f0e68c' lt 1 lw 1 # khaki"
write(99,"(a)") "set style line 65  lc rgb '#bdb76b' lt 1 lw 1 # dark-khaki"
write(99,"(a)") "set style line 66  lc rgb '#b8860b' lt 1 lw 1 # dark-goldenrod"
write(99,"(a)") "set style line 67  lc rgb '#f5f5dc' lt 1 lw 1 # beige"
write(99,"(a)") "set style line 68  lc rgb '#a08020' lt 1 lw 1 # olive"
write(99,"(a)") "set style line 69  lc rgb '#ffa500' lt 1 lw 1 # orange"
write(99,"(a)") "set style line 70  lc rgb '#ee82ee' lt 1 lw 1 # violet"
write(99,"(a)") "set style line 71  lc rgb '#9400d3' lt 1 lw 1 # dark-violet"
write(99,"(a)") "set style line 72  lc rgb '#dda0dd' lt 1 lw 1 # plum"
write(99,"(a)") "set style line 73  lc rgb '#905040' lt 1 lw 1 # dark-plum"
write(99,"(a)") "set style line 74  lc rgb '#556b2f' lt 1 lw 1 # dark-olivegreen"
write(99,"(a)") "set style line 75  lc rgb '#801400' lt 1 lw 1 # orangered4"
write(99,"(a)") "set style line 76  lc rgb '#801414' lt 1 lw 1 # brown4"
write(99,"(a)") "set style line 77  lc rgb '#804014' lt 1 lw 1 # sienna4"
write(99,"(a)") "set style line 78  lc rgb '#804080' lt 1 lw 1 # orchid4"
write(99,"(a)") "set style line 79  lc rgb '#8060c0' lt 1 lw 1 # mediumpurple3"
write(99,"(a)") "set style line 80  lc rgb '#8060ff' lt 1 lw 1 # slateblue1"
write(99,"(a)") "set style line 81  lc rgb '#808000' lt 1 lw 1 # yellow4"
write(99,"(a)") "set style line 82  lc rgb '#ff8040' lt 1 lw 1 # sienna1"
write(99,"(a)") "set style line 83  lc rgb '#ffa040' lt 1 lw 1 # tan1"
write(99,"(a)") "set style line 84  lc rgb '#ffa060' lt 1 lw 1 # sandybrown"
write(99,"(a)") "set style line 85  lc rgb '#ffa070' lt 1 lw 1 # light-salmon"
write(99,"(a)") "set style line 86  lc rgb '#ffc0c0' lt 1 lw 1 # pink"
write(99,"(a)") "set style line 87  lc rgb '#ffff80' lt 1 lw 1 # khaki1"
write(99,"(a)") "set style line 88  lc rgb '#ffffc0' lt 1 lw 1 # lemonchiffon"
write(99,"(a)") "set style line 89  lc rgb '#cdb79e' lt 1 lw 1 # bisque"
write(99,"(a)") "set style line 90  lc rgb '#f0fff0' lt 1 lw 1 # honeydew"
write(99,"(a)") "set style line 91  lc rgb '#a0b6cd' lt 1 lw 1 # slategrey"
write(99,"(a)") "set style line 92  lc rgb '#c1ffc1' lt 1 lw 1 # seagreen"
write(99,"(a)") "set style line 93  lc rgb '#cdc0b0' lt 1 lw 1 # antiquewhite"
write(99,"(a)") "set style line 94  lc rgb '#7cff40' lt 1 lw 1 # chartreuse"
write(99,"(a)") "set style line 95  lc rgb '#a0ff20' lt 1 lw 1 # greenyellow"
write(99,"(a)") "set style line 96  lc rgb '#bebebe' lt 1 lw 1 # gray"
write(99,"(a)") "set style line 97  lc rgb '#d3d3d3' lt 1 lw 1 # light-gray"
write(99,"(a)") "set style line 98  lc rgb '#d3d3d3' lt 1 lw 1 # light-grey"
write(99,"(a)") "set style line 99  lc rgb '#a0a0a0' lt 1 lw 1 # dark-gray"
write(99,"(a)") "set style line 990 lc rgb '#a0b6cd' lt 1 lw 1 # slategray"
write(99,"(a)") "set style line 991 lc rgb '#F0F8FF' lt 1 lw 1 # aliceblue"
write(99,"(a)") "set style line 992 lc rgb '#FAEBD7' lt 1 lw 1 # antiquewhite"
write(99,"(a)") "set style line 993 lc rgb '#00FFFF' lt 1 lw 1 # aqua"
write(99,"(a)") "set style line 994 lc rgb '#7FFFD4' lt 1 lw 1 # aquamarine"
write(99,"(a)") "set style line 995 lc rgb '#F0FFFF' lt 1 lw 1 # azure"
write(99,"(a)") "set style line 996 lc rgb '#F5F5DC' lt 1 lw 1 # beige"
write(99,"(a)") "set style line 997 lc rgb '#FFE4C4' lt 1 lw 1 # bisque"
write(99,"(a)") "set style line 998 lc rgb '#000000' lt 1 lw 1 # black"
write(99,"(a)") "set style line 999 lc rgb '#FFEBCD' lt 1 lw 1 # blanchedalmond"
write(99,"(a)") "set style line 199 lc rgb '#0000FF' lt 1 lw 1 # blue"
write(99,"(a)") "set style line 111 lc rgb '#8A2BE2' lt 1 lw 1 # blueviolet"
write(99,"(a)") "set style line 112 lc rgb '#A52A2A' lt 1 lw 1 # brown"
write(99,"(a)") "set style line 113 lc rgb '#DEB887' lt 1 lw 1 # burlywood"
write(99,"(a)") "set style line 114 lc rgb '#5F9EA0' lt 1 lw 1 # cadetblue"
write(99,"(a)") "set style line 115 lc rgb '#7FFF00' lt 1 lw 1 # chartreuse"
write(99,"(a)") "set style line 116 lc rgb '#D2691E' lt 1 lw 1 # chocolate"
write(99,"(a)") "set style line 117 lc rgb '#FF7F50' lt 1 lw 1 # coral"
write(99,"(a)") "set style line 118 lc rgb '#6495ED' lt 1 lw 1 # cornflowerblue"
write(99,"(a)") "set style line 119 lc rgb '#FFF8DC' lt 1 lw 1 # cornsilk"
write(99,"(a)") "set style line 120 lc rgb '#DC143C' lt 1 lw 1 # crimson"
write(99,"(a)") "set style line 121 lc rgb '#00FFFF' lt 1 lw 1 # cyan"
write(99,"(a)") "set style line 122 lc rgb '#00008B' lt 1 lw 1 # darkblue"
write(99,"(a)") "set style line 123 lc rgb '#008B8B' lt 1 lw 1 # darkcyan"
write(99,"(a)") "set style line 124 lc rgb '#B8860B' lt 1 lw 1 # darkgoldenrod"
write(99,"(a)") "set style line 125 lc rgb '#A9A9A9' lt 1 lw 1 # darkgray"
write(99,"(a)") "set style line 126 lc rgb '#006400' lt 1 lw 1 # darkgreen"
write(99,"(a)") "set style line 127 lc rgb '#BDB76B' lt 1 lw 1 # darkkhaki"
write(99,"(a)") "set style line 128 lc rgb '#8B008B' lt 1 lw 1 # darkmagenta"
write(99,"(a)") "set style line 129 lc rgb '#556B2F' lt 1 lw 1 # darkolivegreen"
write(99,"(a)") "set style line 130 lc rgb '#FF8C00' lt 1 lw 1 # darkorange"
write(99,"(a)") "set style line 131 lc rgb '#9932CC' lt 1 lw 1 # darkorchid"
write(99,"(a)") "set style line 132 lc rgb '#8B0000' lt 1 lw 1 # darkred"
write(99,"(a)") "set style line 133 lc rgb '#E9967A' lt 1 lw 1 # darksalmon"
write(99,"(a)") "set style line 134 lc rgb '#8FBC8F' lt 1 lw 1 # darkseagreen"
write(99,"(a)") "set style line 135 lc rgb '#483D8B' lt 1 lw 1 # darkslateblue"
write(99,"(a)") "set style line 136 lc rgb '#2F4F4F' lt 1 lw 1 # darkslategray"
write(99,"(a)") "set style line 137 lc rgb '#00CED1' lt 1 lw 1 # darkturquoise"
write(99,"(a)") "set style line 138 lc rgb '#9400D3' lt 1 lw 1 # darkviolet"
write(99,"(a)") "set style line 139 lc rgb '#FF1493' lt 1 lw 1 # deeppink"
write(99,"(a)") "set style line 140 lc rgb '#00BFFF' lt 1 lw 1 # deepskyblue"
write(99,"(a)") "set style line 141 lc rgb '#696969' lt 1 lw 1 # dimgray"
write(99,"(a)") "set style line 142 lc rgb '#1E90FF' lt 1 lw 1 # dodgerblue"
write(99,"(a)") "set style line 143 lc rgb '#B22222' lt 1 lw 1 # firebrick"
write(99,"(a)") "set style line 144 lc rgb '#FFFAF0' lt 1 lw 1 # floralwhite"
write(99,"(a)") "set style line 145 lc rgb '#228B22' lt 1 lw 1 # forestgreen"
write(99,"(a)") "set style line 146 lc rgb '#FF00FF' lt 1 lw 1 # fuchsia"
write(99,"(a)") "set style line 147 lc rgb '#DCDCDC' lt 1 lw 1 # gainsboro"
write(99,"(a)") "set style line 148 lc rgb '#F8F8FF' lt 1 lw 1 # ghostwhite"
write(99,"(a)") "set style line 149 lc rgb '#FFD700' lt 1 lw 1 # gold"
write(99,"(a)") "set style line 150 lc rgb '#DAA520' lt 1 lw 1 # goldenrod"
write(99,"(a)") "set style line 151 lc rgb '#7F7F7F' lt 1 lw 1 # gray"
write(99,"(a)") "set style line 152 lc rgb '#008000' lt 1 lw 1 # green"
write(99,"(a)") "set style line 153 lc rgb '#ADFF2F' lt 1 lw 1 # greenyellow"
write(99,"(a)") "set style line 154 lc rgb '#F0FFF0' lt 1 lw 1 # honeydew"
write(99,"(a)") "set style line 155 lc rgb '#FF69B4' lt 1 lw 1 # hotpink"
write(99,"(a)") "set style line 156 lc rgb '#CD5C5C' lt 1 lw 1 # indianred"
write(99,"(a)") "set style line 157 lc rgb '#4B0082' lt 1 lw 1 # indigo"
write(99,"(a)") "set style line 158 lc rgb '#FFFFF0' lt 1 lw 1 # ivory"
write(99,"(a)") "set style line 159 lc rgb '#F0E68C' lt 1 lw 1 # khaki"
write(99,"(a)") "set style line 160 lc rgb '#E6E6FA' lt 1 lw 1 # lavender"
write(99,"(a)") "set style line 161 lc rgb '#FFF0F5' lt 1 lw 1 # lavenderblush"
write(99,"(a)") "set style line 162 lc rgb '#7CFC00' lt 1 lw 1 # lawngreen"
write(99,"(a)") "set style line 163 lc rgb '#FFFACD' lt 1 lw 1 # lemonchiffon"
write(99,"(a)") "set style line 164 lc rgb '#ADD8E6' lt 1 lw 1 # lightblue"
write(99,"(a)") "set style line 165 lc rgb '#F08080' lt 1 lw 1 # lightcoral"
write(99,"(a)") "set style line 166 lc rgb '#E0FFFF' lt 1 lw 1 # lightcyan"
write(99,"(a)") "set style line 167 lc rgb '#FAFAD2' lt 1 lw 1 # lightgoldenrodyellow"
write(99,"(a)") "set style line 168 lc rgb '#90EE90' lt 1 lw 1 # lightgreen"
write(99,"(a)") "set style line 169 lc rgb '#D3D3D3' lt 1 lw 1 # lightgrey"
write(99,"(a)") "set style line 170 lc rgb '#FFB6C1' lt 1 lw 1 # lightpink"
write(99,"(a)") "set style line 171 lc rgb '#FFA07A' lt 1 lw 1 # lightsalmon"
write(99,"(a)") "set style line 172 lc rgb '#20B2AA' lt 1 lw 1 # lightseagreen"
write(99,"(a)") "set style line 173 lc rgb '#87CEFA' lt 1 lw 1 # lightskyblue"
write(99,"(a)") "set style line 174 lc rgb '#778899' lt 1 lw 1 # lightslategray"
write(99,"(a)") "set style line 175 lc rgb '#B0C4DE' lt 1 lw 1 # lightsteelblue"
write(99,"(a)") "set style line 176 lc rgb '#FFFFE0' lt 1 lw 1 # lightyellow"
write(99,"(a)") "set style line 177 lc rgb '#00FF00' lt 1 lw 1 # lime"
write(99,"(a)") "set style line 178 lc rgb '#32CD32' lt 1 lw 1 # limegreen"
write(99,"(a)") "set style line 179 lc rgb '#FAF0E6' lt 1 lw 1 # linen"
write(99,"(a)") "set style line 180 lc rgb '#FF00FF' lt 1 lw 1 # magenta"
write(99,"(a)") "set style line 181 lc rgb '#800000' lt 1 lw 1 # maroon"
write(99,"(a)") "set style line 182 lc rgb '#66CDAA' lt 1 lw 1 # mediumaquamarine"
write(99,"(a)") "set style line 183 lc rgb '#0000CD' lt 1 lw 1 # mediumblue"
write(99,"(a)") "set style line 184 lc rgb '#BA55D3' lt 1 lw 1 # mediumorchid"
write(99,"(a)") "set style line 185 lc rgb '#9370DB' lt 1 lw 1 # mediumpurple"
write(99,"(a)") "set style line 186 lc rgb '#3CB371' lt 1 lw 1 # mediumseagreen"
write(99,"(a)") "set style line 187 lc rgb '#7B68EE' lt 1 lw 1 # mediumslateblue"
write(99,"(a)") "set style line 188 lc rgb '#00FA9A' lt 1 lw 1 # mediumspringgreen"
write(99,"(a)") "set style line 189 lc rgb '#48D1CC' lt 1 lw 1 # mediumturquoise"
write(99,"(a)") "set style line 190 lc rgb '#C71585' lt 1 lw 1 # mediumvioletred"
write(99,"(a)") "set style line 191 lc rgb '#191970' lt 1 lw 1 # midnightblue"
write(99,"(a)") "set style line 192 lc rgb '#F5FFFA' lt 1 lw 1 # mintcream"
write(99,"(a)") "set style line 193 lc rgb '#FFE4E1' lt 1 lw 1 # mistyrose"
write(99,"(a)") "set style line 194 lc rgb '#FFE4B5' lt 1 lw 1 # moccasin"
write(99,"(a)") "set style line 195 lc rgb '#FFDEAD' lt 1 lw 1 # navajowhite"
write(99,"(a)") "set style line 196 lc rgb '#000080' lt 1 lw 1 # navy"
write(99,"(a)") "set style line 197 lc rgb '#9FAFDF' lt 1 lw 1 # navyblue"
write(99,"(a)") "set style line 198 lc rgb '#FDF5E6' lt 1 lw 1 # oldlace"
write(99,"(a)") "set style line 199 lc rgb '#808000' lt 1 lw 1 # olive"
write(99,"(a)") "set style line 200 lc rgb '#6B8E23' lt 1 lw 1 # olivedrab"
write(99,"(a)") "set style line 201 lc rgb '#FFA500' lt 1 lw 1 # orange"
write(99,"(a)") "set style line 202 lc rgb '#FF4500' lt 1 lw 1 # orangered"
write(99,"(a)") "set style line 203 lc rgb '#DA70D6' lt 1 lw 1 # orchid"
write(99,"(a)") "set style line 204 lc rgb '#EEE8AA' lt 1 lw 1 # palegoldenrod"
write(99,"(a)") "set style line 205 lc rgb '#98FB98' lt 1 lw 1 # palegreen"
write(99,"(a)") "set style line 206 lc rgb '#AFEEEE' lt 1 lw 1 # paleturquoise"
write(99,"(a)") "set style line 207 lc rgb '#DB7093' lt 1 lw 1 # palevioletred"
write(99,"(a)") "set style line 208 lc rgb '#FFEFD5' lt 1 lw 1 # papayawhip"
write(99,"(a)") "set style line 209 lc rgb '#FFDAB9' lt 1 lw 1 # peachpuff"
write(99,"(a)") "set style line 299 lc rgb '#CD853F' lt 1 lw 1 # peru"
write(99,"(a)") "set style line 211 lc rgb '#FFC0CB' lt 1 lw 1 # pink"
write(99,"(a)") "set style line 212 lc rgb '#DDA0DD' lt 1 lw 1 # plum"
write(99,"(a)") "set style line 213 lc rgb '#B0E0E6' lt 1 lw 1 # powderblue"
write(99,"(a)") "set style line 214 lc rgb '#800080' lt 1 lw 1 # purple"
write(99,"(a)") "set style line 215 lc rgb '#FF0000' lt 1 lw 1 # red"
write(99,"(a)") "set style line 216 lc rgb '#BC8F8F' lt 1 lw 1 # rosybrown"
write(99,"(a)") "set style line 217 lc rgb '#4169E1' lt 1 lw 1 # royalblue"
write(99,"(a)") "set style line 218 lc rgb '#8B4513' lt 1 lw 1 # saddlebrown"
write(99,"(a)") "set style line 219 lc rgb '#FA8072' lt 1 lw 1 # salmon"
write(99,"(a)") "set style line 220 lc rgb '#F4A460' lt 1 lw 1 # sandybrown"
write(99,"(a)") "set style line 221 lc rgb '#2E8B57' lt 1 lw 1 # seagreen"
write(99,"(a)") "set style line 222 lc rgb '#FFF5EE' lt 1 lw 1 # seashell"
write(99,"(a)") "set style line 223 lc rgb '#A0522D' lt 1 lw 1 # sienna"
write(99,"(a)") "set style line 224 lc rgb '#C0C0C0' lt 1 lw 1 # silver"
write(99,"(a)") "set style line 225 lc rgb '#87CEEB' lt 1 lw 1 # skyblue"
write(99,"(a)") "set style line 226 lc rgb '#6A5ACD' lt 1 lw 1 # slateblue"
write(99,"(a)") "set style line 227 lc rgb '#708090' lt 1 lw 1 # slategray"
write(99,"(a)") "set style line 228 lc rgb '#FFFAFA' lt 1 lw 1 # snow"
write(99,"(a)") "set style line 229 lc rgb '#00FF7F' lt 1 lw 1 # springgreen"
write(99,"(a)") "set style line 230 lc rgb '#4682B4' lt 1 lw 1 # steelblue"
write(99,"(a)") "set style line 231 lc rgb '#D2B48C' lt 1 lw 1 # tan"
write(99,"(a)") "set style line 232 lc rgb '#008080' lt 1 lw 1 # teal"
write(99,"(a)") "set style line 233 lc rgb '#D8BFD8' lt 1 lw 1 # thistle"
write(99,"(a)") "set style line 234 lc rgb '#FF6347' lt 1 lw 1 # tomato"
write(99,"(a)") "set style line 235 lc rgb '#40E0D0' lt 1 lw 1 # turquoise"
write(99,"(a)") "set style line 236 lc rgb '#EE82EE' lt 1 lw 1 # violet"
write(99,"(a)") "set style line 237 lc rgb '#F5DEB3' lt 1 lw 1 # wheat"
write(99,"(a)") "set style line 238 lc rgb '#FFFFFF' lt 1 lw 1 # white"
write(99,"(a)") "set style line 239 lc rgb '#F5F5F5' lt 1 lw 1 # whitesmoke"
write(99,"(a)") "set style line 240 lc rgb '#FFFF00' lt 1 lw 1 # yellow"
write(99,"(a)") "set style line 241 lc rgb '#9ACD32' lt 1 lw 1 # yellowgreen"
close(99)
end subroutine
!
!     %--------------------------%
!        delete linestyle.gnu
!     %--------------------------%
!
subroutine delete_linestyle()
logical :: alive
inquire(file="linestyle.gnu", exist=alive)
if (alive) then
open(unit=99, file="linestyle.gnu", status="old")
close(unit=99, status="delete")
end if
end subroutine

end module

