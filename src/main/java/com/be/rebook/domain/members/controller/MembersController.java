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
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/members")
public class MembersController {
    private final ReissueService reissueService;

    private final MemberService memberService;

    public MembersController(ReissueService reissueService,
                             MemberService memberService){
        this.reissueService = reissueService;
        this.memberService = memberService;
    }

    @PatchMapping
    public ResponseEntity<BaseResponse<Members>> updateUser(HttpServletRequest request) {

        String accessToken = request.getHeader("Authorization").substring(7);

        if (accessToken == null) {
            //no_token
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        UpdateDTO membersUpdateDTO = new UpdateDTO();
        if (request.getContentType().equals(MediaType.APPLICATION_JSON_VALUE)) {
            try {
                InputStream inputStream = request.getInputStream();
                ObjectMapper mapper = new ObjectMapper();
                Map<String, String> requestMap = mapper.readValue(inputStream, Map.class);

                membersUpdateDTO.setNickname(requestMap.get("nickname"));
                membersUpdateDTO.setUniversity(requestMap.get("university"));
                membersUpdateDTO.setMajors(requestMap.get("majors"));

            } catch (IOException e) {
                throw new AuthenticationServiceException("Error parsing JSON request", e);
            }
        }

        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUser(accessToken, membersUpdateDTO)));
    }

    //요청 보낼때 헤더에 키: access, 값 : 로컬스토리지에서 관리되는 Authorization토큰 값 넘어와야함
    @DeleteMapping
    public ResponseEntity<BaseResponse<Members>> deleteUser(HttpServletRequest request) {
        String accessToken = request.getHeader("Authorization").substring(7);

        if (accessToken == null) {
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

    @GetMapping
    public ResponseEntity<BaseResponse<UserinfoDTO>> showUserinfos(HttpServletRequest request){
        String accessToken = request.getHeader("Authorization").substring(7);

        if (accessToken == null){
            throw new BaseException(ErrorCode.NO_TOKEN_CONTENT);
        }

        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getUserinfo(accessToken)));
    }
}
