package com.be.rebook.domain.members.controller;

import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.service.MemberService;
import com.be.rebook.domain.members.dto.UpdateDTO;
import com.be.rebook.domain.members.service.ReissueService;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/members")
public class MembersController {

    private static final Logger memberLogger = LoggerFactory.getLogger(MembersController.class);
    private final ReissueService reissueService;

    private final MemberService memberService;


    public MembersController(ReissueService reissueService,
                             MemberService memberService){
        this.reissueService = reissueService;
        this.memberService = memberService;
    }

    //todo : BaseResponse 추가하기
    @PatchMapping
    public ResponseEntity<Members> updateUser(HttpServletRequest request, UpdateDTO membersUpdateDTO) {
        memberLogger.info("회원 정보 업데이트 시작");
        String accessToken = request.getHeader("access");
        String refreshToken = null;
        for(Cookie c : request.getCookies()){
            if(c.getName().equals("refresh"))
                refreshToken = c.getValue();
        }

        if (accessToken == null || refreshToken == null) {
            //no_token
            memberLogger.error("회원 정보 업데이트 실패 : 토큰 없음");
            return ResponseEntity.status(401).build();
        }

        return memberService.updateUser(accessToken, membersUpdateDTO);
    }

    //todo : BaseResponse 추가하기
    //요청 보낼때 헤더에 키: access, 값 : 로컬스토리지에서 관리되는 access토큰 값 넘어와야함
    @DeleteMapping
    public ResponseEntity<Void> deleteUser(HttpServletRequest request) {
        memberLogger.info("회원 탈퇴 로직 시작");
        String accessToken = request.getHeader("access");
        String refreshToken = null;
        for(Cookie c : request.getCookies()){
            if(c.getName().equals("refresh"))
                refreshToken = c.getValue();
        }

        if (accessToken == null || refreshToken == null) {
            memberLogger.error("회원 탈퇴 실패 : 토큰 없음 코드 {}", 401);
            return ResponseEntity.status(401).build();
        }

        return memberService.deleteUser(accessToken);
    }

    //todo : BaseResponse 추가하기
    @PostMapping("/refreshtoken")
    public ResponseEntity<?> reissue(HttpServletRequest request, HttpServletResponse response) {
        return reissueService.reissueToken(request, response);
    }
}
