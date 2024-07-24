package com.be.rebook.domain.members.controller;

import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.service.MemberService;
import com.be.rebook.domain.members.dto.UpdateDTO;
import com.be.rebook.domain.members.service.ReissueService;
import com.be.rebook.global.config.BaseResponse;
import com.be.rebook.global.exception.ErrorCode;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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

    @PatchMapping
    public BaseResponse<Members> updateUser(HttpServletRequest request, UpdateDTO membersUpdateDTO) {
        memberLogger.info("회원 정보 업데이트 시작");
        String accessToken = request.getHeader("access");
        String refreshToken = null;
        for(Cookie c : request.getCookies()){
            if(c.getName().equals("refresh"))
                refreshToken = c.getValue();
        }

        HttpStatus returnStatus = null;
        String returnCode = null;
        String returnMessage = null;

        if (accessToken == null || refreshToken == null) {
            //no_token
            memberLogger.error("회원 정보 업데이트 실패 : 토큰 없음");
            returnStatus = ErrorCode.NO_TOKEN_CONTENT.getStatus();
            returnCode = returnStatus + " failed";
            returnMessage = ErrorCode.NO_TOKEN_CONTENT.getMessage();
            return new BaseResponse<>(returnStatus, returnCode, returnMessage, null);
        }

        return memberService.updateUser(accessToken, membersUpdateDTO);
    }

    //요청 보낼때 헤더에 키: access, 값 : 로컬스토리지에서 관리되는 access토큰 값 넘어와야함
    @DeleteMapping
    public BaseResponse<Members> deleteUser(HttpServletRequest request) {
        memberLogger.info("회원 탈퇴 로직 시작");
        String accessToken = request.getHeader("access");
        String refreshToken = null;
        for(Cookie c : request.getCookies()){
            if(c.getName().equals("refresh"))
                refreshToken = c.getValue();
        }

        if (accessToken == null || refreshToken == null) {
            memberLogger.error("회원 탈퇴 실패 : 토큰 없음 코드 {}", ErrorCode.NO_TOKEN_CONTENT);
            return new BaseResponse<>(ErrorCode.NO_TOKEN_CONTENT.getStatus(),
                    ErrorCode.NO_TOKEN_CONTENT.toString() + " failed",
                    ErrorCode.NO_TOKEN_CONTENT.getMessage(),
                    null);
        }

        return memberService.deleteUser(accessToken);
    }

    @PostMapping("/refreshtoken")
    public BaseResponse<?> reissue(HttpServletRequest request, HttpServletResponse response) {
        return reissueService.reissueToken(request, response);
    }

    @GetMapping("/universities")
    public BaseResponse<List<String>> searchUniversities(@RequestParam("unvToSearch") String unvToSearch){
        return memberService.getUniversitiesList(unvToSearch);
    }
}
