package com.be.rebook.domain.members.controller;

import com.be.rebook.domain.members.dto.UserinfoDTO;
import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.entity.RefreshTokens;
import com.be.rebook.domain.members.service.MemberService;
import com.be.rebook.domain.members.dto.UpdateDTO;
import com.be.rebook.domain.members.service.ReissueService;
import com.be.rebook.global.config.BaseResponse;
import com.be.rebook.global.exception.BaseException;
import com.be.rebook.global.exception.ErrorCode;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
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
    public ResponseEntity<BaseResponse<Members>> updateUser(HttpServletRequest request, UpdateDTO membersUpdateDTO) {
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
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUser(accessToken, membersUpdateDTO)));
    }

    //요청 보낼때 헤더에 키: access, 값 : 로컬스토리지에서 관리되는 access토큰 값 넘어와야함
    @DeleteMapping
    public ResponseEntity<BaseResponse<Members>> deleteUser(HttpServletRequest request) {
        memberLogger.info("회원 탈퇴 로직 시작");
        String accessToken = request.getHeader("access");
        String refreshToken = null;
        for(Cookie c : request.getCookies()){
            if(c.getName().equals("refresh"))
                refreshToken = c.getValue();
        }

        if (accessToken == null || refreshToken == null) {
            memberLogger.error("회원 탈퇴 실패 : 토큰 없음 코드 {}", ErrorCode.NO_TOKEN_CONTENT);
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        return ResponseEntity.ok().body(new BaseResponse<>(memberService.deleteUser(accessToken)));
    }

    @PostMapping("/refreshtoken")
    public ResponseEntity<BaseResponse<RefreshTokens>> reissue(HttpServletRequest request, HttpServletResponse response) {
        return ResponseEntity.ok().body(new BaseResponse<>(reissueService.reissueToken(request, response)));
    }

    @GetMapping("/universities")
    public ResponseEntity<BaseResponse<List<String>>> searchUniversities(@RequestParam("unvToSearch") String unvToSearch){
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getUniversitiesList(unvToSearch)));
    }

    @GetMapping("/majors")
    public ResponseEntity<BaseResponse<List<String>>> searchMajors(@RequestParam("majorToSearch") String majorToSearch){
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getMajorsList(majorToSearch)));
    }

    //todo : 마이페이지에서 보여줄 회원 정보 가져오기
    @GetMapping
    public ResponseEntity<BaseResponse<UserinfoDTO>> showUserinfos(HttpServletRequest request){
        memberLogger.info("회원 정보 조회 로직 시작");
        String accessToken = request.getHeader("access");

        if (accessToken == null){
            memberLogger.error("회원 정보 조회 실패 : 토큰 없음 코드 {}", ErrorCode.NO_TOKEN_CONTENT);
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getUserinfo(accessToken)));
    }
}
