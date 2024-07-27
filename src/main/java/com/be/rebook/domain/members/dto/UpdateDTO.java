package com.be.rebook.domain.members.dto;

import lombok.Getter;
import lombok.Setter;

// 업데이트 할 때 프론트에서
// 자신의 이름은 그냥 입력하고
// 대학교 항목은 커서 누르면 아래로 쭉 메뉴 나와서 예를들어 서울대학교면
// 서울ㄷ까지 치면 아래 서울대학교 떠서 그거 클릭하면 서울대학교로 바뀜
// 아래 뜨는 항목들은 미리 대학교 테이블에서 읽어와서 관련성 높은 순서대로 출력
//전공 여러개 선택가능함
//예시
//닉네임 : toby
//대학교 : 고려대학교
//전공들 : 경영학과,컴퓨터과학과
@Setter
@Getter
public class UpdateDTO {
    private String nickname;
    private String university;
    private String majors;
}
