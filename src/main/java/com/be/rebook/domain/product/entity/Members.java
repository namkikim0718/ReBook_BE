package com.be.rebook.domain.product.entity;

import jakarta.persistence.*;
import lombok.Builder;
import lombok.Getter;

@Entity
@Getter
public class Members {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "member_id")
    private Long memberId;

    private String username; //아이디
    private String password;
    private String role;

    private String nickname; // 닉네임, 채팅시에 이거 우선 표시해주고 널값이면 username 표시하기
    private Long university; // 숫자 코드 저장하고 실제 데이터는 university에 있음
    private String majors; //, 콤마로 구분해서 여러개 넣을수 있음 ex.("1,2,5,7") 숫자는 해당 전공에 대한 아이디

    public Members(){}
    @Builder(toBuilder = true)
    public Members(Long memberId,
                   String username,
                   String password,
                   String role,
                   String nickname,
                   Long university,
                   String majors) {
        this.memberId = memberId;
        this.username = username;
        this.password = password;
        this.role = role;
        this.nickname = nickname;
        this.university = university;
        this.majors = majors;
    }
}
