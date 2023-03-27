; ModuleID = 'df'
source_filename = "<string>"

@decode_table_0 = global <16 x ptr> [ptr blockaddress(@f, %0), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %1), ptr false]
@decode_table_1 = global <16 x ptr> [ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %2), ptr false]
@decode_table_2 = global <16 x ptr> [ptr blockaddress(@f, %4), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_3 = global <16 x ptr> [ptr blockaddress(@f, %31), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_4 = global <16 x ptr> [ptr blockaddress(@f, %49), ptr blockaddress(@f, %55), ptr blockaddress(@f, %61), ptr blockaddress(@f, %67), ptr blockaddress(@f, %73), ptr blockaddress(@f, %79), ptr blockaddress(@f, %85), ptr blockaddress(@f, %91), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %97), ptr false]
@decode_table_5 = global <16 x ptr> [ptr blockaddress(@f, %105), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_6 = global <16 x ptr> [ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %129), ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_7 = global <16 x ptr> [ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %136), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false]
@decode_table_8 = global <16 x ptr> [ptr false, ptr blockaddress(@f, %130), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %137), ptr false]
@decode_table_9 = global <16 x ptr> [ptr blockaddress(@f, %147), ptr blockaddress(@f, %153), ptr blockaddress(@f, %159), ptr blockaddress(@f, %165), ptr blockaddress(@f, %171), ptr blockaddress(@f, %177), ptr blockaddress(@f, %183), ptr blockaddress(@f, %189), ptr false, ptr false, ptr false, ptr false, ptr false, ptr false, ptr blockaddress(@f, %195), ptr false]
@decode_table_10 = global <16 x ptr> [ptr blockaddress(@f, %7), ptr blockaddress(@f, %12), ptr blockaddress(@f, %15), ptr blockaddress(@f, %20), ptr blockaddress(@f, %25), ptr blockaddress(@f, %32), ptr blockaddress(@f, %38), ptr blockaddress(@f, %43), ptr blockaddress(@f, %98), ptr blockaddress(@f, %106), ptr blockaddress(@f, %110), ptr blockaddress(@f, %113), ptr blockaddress(@f, %118), ptr blockaddress(@f, %125), ptr blockaddress(@f, %140), ptr blockaddress(@f, %196)]

define void @f(i10 %param_0) {
  ret void

1:                                                ; preds = %2
  ret void

2:                                                ; preds = %4
  %3 = trunc i10 %param_0 to i1
  indirectbr i1 %3, [label %0, label %1]

4:                                                ; preds = %7
  %5 = lshr exact i10 %param_0, 1
  %6 = trunc i10 %5 to i1
  indirectbr i1 %6, [label %2]

7:                                                ; preds = %198
  %8 = lshr exact i10 %param_0, 2
  %9 = trunc i10 %8 to i1
  indirectbr i1 %9, [label %4]

10:                                               ; No predecessors!
  %11 = trunc i10 %param_0 to i3
  ret void

12:                                               ; preds = %198
  ret void

13:                                               ; No predecessors!
  %14 = trunc i10 %param_0 to i3
  ret void

15:                                               ; preds = %198
  ret void

16:                                               ; No predecessors!
  %17 = lshr exact i10 %param_0, i8 2
  %18 = trunc i10 %17 to i1
  %19 = trunc i10 %param_0 to i2
  ret void

20:                                               ; preds = %198
  ret void

21:                                               ; No predecessors!
  %22 = lshr exact i10 %param_0, i8 2
  %23 = trunc i10 %22 to i1
  %24 = trunc i10 %param_0 to i2
  ret void

25:                                               ; preds = %198
  ret void

26:                                               ; No predecessors!
  %27 = lshr exact i10 %param_0, i8 2
  %28 = trunc i10 %27 to i1
  %29 = lshr exact i10 %param_0, i9 1
  %30 = trunc i10 %29 to i1
  ret void

31:                                               ; preds = %32
  ret void

32:                                               ; preds = %198
  %33 = trunc i10 %param_0 to i1
  indirectbr i1 %33, [label %31]

34:                                               ; No predecessors!
  %35 = lshr exact i10 %param_0, i8 2
  %36 = trunc i10 %35 to i1
  %37 = trunc i10 %param_0 to i2
  ret void

38:                                               ; preds = %198
  ret void

39:                                               ; No predecessors!
  %40 = lshr exact i10 %param_0, i8 2
  %41 = trunc i10 %40 to i1
  %42 = trunc i10 %param_0 to i2
  ret void

43:                                               ; preds = %198
  ret void

44:                                               ; No predecessors!
  %45 = lshr exact i10 %param_0, i8 2
  %46 = trunc i10 %45 to i1
  %47 = lshr exact i10 %param_0, i9 1
  %48 = trunc i10 %47 to i1
  ret void

49:                                               ; preds = %98
  ret void

50:                                               ; No predecessors!
  %51 = lshr exact i10 %param_0, i8 2
  %52 = trunc i10 %51 to i1
  %53 = lshr exact i10 %param_0, i9 1
  %54 = trunc i10 %53 to i1
  ret void

55:                                               ; preds = %98
  ret void

56:                                               ; No predecessors!
  %57 = lshr exact i10 %param_0, i8 2
  %58 = trunc i10 %57 to i1
  %59 = lshr exact i10 %param_0, i9 1
  %60 = trunc i10 %59 to i1
  ret void

61:                                               ; preds = %98
  ret void

62:                                               ; No predecessors!
  %63 = lshr exact i10 %param_0, i8 2
  %64 = trunc i10 %63 to i1
  %65 = lshr exact i10 %param_0, i9 1
  %66 = trunc i10 %65 to i1
  ret void

67:                                               ; preds = %98
  ret void

68:                                               ; No predecessors!
  %69 = lshr exact i10 %param_0, i8 2
  %70 = trunc i10 %69 to i1
  %71 = lshr exact i10 %param_0, i9 1
  %72 = trunc i10 %71 to i1
  ret void

73:                                               ; preds = %98
  ret void

74:                                               ; No predecessors!
  %75 = lshr exact i10 %param_0, i8 2
  %76 = trunc i10 %75 to i1
  %77 = lshr exact i10 %param_0, i9 1
  %78 = trunc i10 %77 to i1
  ret void

79:                                               ; preds = %98
  ret void

80:                                               ; No predecessors!
  %81 = lshr exact i10 %param_0, i8 2
  %82 = trunc i10 %81 to i1
  %83 = lshr exact i10 %param_0, i9 1
  %84 = trunc i10 %83 to i1
  ret void

85:                                               ; preds = %98
  ret void

86:                                               ; No predecessors!
  %87 = lshr exact i10 %param_0, i8 2
  %88 = trunc i10 %87 to i1
  %89 = lshr exact i10 %param_0, i9 1
  %90 = trunc i10 %89 to i1
  ret void

91:                                               ; preds = %98
  ret void

92:                                               ; No predecessors!
  %93 = lshr exact i10 %param_0, i8 2
  %94 = trunc i10 %93 to i1
  %95 = lshr exact i10 %param_0, i9 1
  %96 = trunc i10 %95 to i1
  ret void

97:                                               ; preds = %98
  ret void

98:                                               ; preds = %198
  %99 = trunc i10 %param_0 to i1
  indirectbr i1 %99, [label %49, label %55, label %61, label %67, label %73, label %79, label %85, label %91, label %97]

100:                                              ; No predecessors!
  %101 = lshr exact i10 %param_0, i8 2
  %102 = trunc i10 %101 to i1
  %103 = lshr exact i10 %param_0, i9 1
  %104 = trunc i10 %103 to i1
  ret void

105:                                              ; preds = %106
  ret void

106:                                              ; preds = %198
  %107 = trunc i10 %param_0 to i1
  indirectbr i1 %107, [label %105]

108:                                              ; No predecessors!
  %109 = trunc i10 %param_0 to i3
  ret void

110:                                              ; preds = %198
  ret void

111:                                              ; No predecessors!
  %112 = trunc i10 %param_0 to i3
  ret void

113:                                              ; preds = %198
  ret void

114:                                              ; No predecessors!
  %115 = lshr exact i10 %param_0, i8 2
  %116 = trunc i10 %115 to i1
  %117 = trunc i10 %param_0 to i2
  ret void

118:                                              ; preds = %198
  ret void

119:                                              ; No predecessors!
  %120 = lshr exact i10 %param_0, i8 2
  %121 = trunc i10 %120 to i1
  %122 = lshr exact i10 %param_0, i9 1
  %123 = trunc i10 %122 to i1
  %124 = trunc i10 %param_0 to i1
  ret void

125:                                              ; preds = %198
  ret void

126:                                              ; No predecessors!
  %127 = lshr exact i10 %param_0, i8 2
  %128 = trunc i10 %127 to i1
  ret void

129:                                              ; preds = %130
  ret void

130:                                              ; preds = %140
  %131 = lshr exact i10 %param_0, 1
  %132 = trunc i10 %131 to i1
  indirectbr i1 %132, [label %129]

133:                                              ; No predecessors!
  %134 = lshr exact i10 %param_0, i8 2
  %135 = trunc i10 %134 to i1
  ret void

136:                                              ; preds = %137
  ret void

137:                                              ; preds = %140
  %138 = lshr exact i10 %param_0, 1
  %139 = trunc i10 %138 to i1
  indirectbr i1 %139, [label %136]

140:                                              ; preds = %198
  %141 = trunc i10 %param_0 to i1
  indirectbr i1 %141, [label %130, label %137]

142:                                              ; No predecessors!
  %143 = lshr exact i10 %param_0, i8 2
  %144 = trunc i10 %143 to i1
  %145 = lshr exact i10 %param_0, i9 1
  %146 = trunc i10 %145 to i1
  ret void

147:                                              ; preds = %196
  ret void

148:                                              ; No predecessors!
  %149 = lshr exact i10 %param_0, i8 2
  %150 = trunc i10 %149 to i1
  %151 = lshr exact i10 %param_0, i9 1
  %152 = trunc i10 %151 to i1
  ret void

153:                                              ; preds = %196
  ret void

154:                                              ; No predecessors!
  %155 = lshr exact i10 %param_0, i8 2
  %156 = trunc i10 %155 to i1
  %157 = lshr exact i10 %param_0, i9 1
  %158 = trunc i10 %157 to i1
  ret void

159:                                              ; preds = %196
  ret void

160:                                              ; No predecessors!
  %161 = lshr exact i10 %param_0, i8 2
  %162 = trunc i10 %161 to i1
  %163 = lshr exact i10 %param_0, i9 1
  %164 = trunc i10 %163 to i1
  ret void

165:                                              ; preds = %196
  ret void

166:                                              ; No predecessors!
  %167 = lshr exact i10 %param_0, i8 2
  %168 = trunc i10 %167 to i1
  %169 = lshr exact i10 %param_0, i9 1
  %170 = trunc i10 %169 to i1
  ret void

171:                                              ; preds = %196
  ret void

172:                                              ; No predecessors!
  %173 = lshr exact i10 %param_0, i8 2
  %174 = trunc i10 %173 to i1
  %175 = lshr exact i10 %param_0, i9 1
  %176 = trunc i10 %175 to i1
  ret void

177:                                              ; preds = %196
  ret void

178:                                              ; No predecessors!
  %179 = lshr exact i10 %param_0, i8 2
  %180 = trunc i10 %179 to i1
  %181 = lshr exact i10 %param_0, i9 1
  %182 = trunc i10 %181 to i1
  ret void

183:                                              ; preds = %196
  ret void

184:                                              ; No predecessors!
  %185 = lshr exact i10 %param_0, i8 2
  %186 = trunc i10 %185 to i1
  %187 = lshr exact i10 %param_0, i9 1
  %188 = trunc i10 %187 to i1
  ret void

189:                                              ; preds = %196
  ret void

190:                                              ; No predecessors!
  %191 = lshr exact i10 %param_0, i8 2
  %192 = trunc i10 %191 to i1
  %193 = lshr exact i10 %param_0, i9 1
  %194 = trunc i10 %193 to i1
  ret void

195:                                              ; preds = %196
  ret void

196:                                              ; preds = %198
  %197 = trunc i10 %param_0 to i1
  indirectbr i1 %197, [label %147, label %153, label %159, label %165, label %171, label %177, label %183, label %189, label %195]

198:                                              ; preds = %201
  %199 = lshr exact i10 %param_0, 3
  %200 = trunc i10 %199 to i1
  indirectbr i1 %200, [label %7, label %12, label %15, label %20, label %25, label %32, label %38, label %43, label %98, label %106, label %110, label %113, label %118, label %125, label %140, label %196]

201:                                              ; No predecessors!
  br label %198
}
