package com.be.rebook.members.controller;

import com.be.rebook.members.service.MemberService;
import org.springframework.http.ResponseEntity;

import com.be.rebook.common.argumentresolver.auth.Auth;
import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.config.BaseResponse;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;


@RestController
@RequestMapping("/members")
public class MembersController {

    private final MemberService memberService;

    private static final String ACCESSTOKEN_HEADER = "Authorization";

    public MembersController(MemberService memberService) {
        this.memberService = memberService;
    }

    @GetMapping("/me")
    public ResponseEntity<BaseResponse<MemberLoginInfo>> getMethodName(@Auth MemberLoginInfo memberLoginInfo) {
        //TODO : 추가 개인정보가 필요하다면 DTO를 만들어서 반환하도록 수정
        return ResponseEntity.ok().body(new BaseResponse<>(memberLoginInfo));
    }
    

    // @PatchMapping
    // public ResponseEntity<BaseResponse<Members>> updateUser(HttpServletRequest request) {

    //     String accessToken = request.getHeader(ACCESSTOKEN_HEADER).substring(7);

    //     UpdateDTO membersUpdateDTO = new UpdateDTO();
    //     if (request.getContentType().equals(MediaType.APPLICATION_JSON_VALUE)) {
    //         try {
    //             InputStream inputStream = request.getInputStream();
    //             ObjectMapper mapper = new ObjectMapper();
    //             Map<String, String> requestMap = mapper.readValue(inputStream, Map.class);

    //             membersUpdateDTO.setNickname(requestMap.get("nickname"));
    //             membersUpdateDTO.setUniversity(requestMap.get("university"));
    //             membersUpdateDTO.setMajors(requestMap.get("majors"));

    //         } catch (IOException e) {
    //             throw new BaseException(ErrorCode.BAD_REQUEST); // TODO: 적절한 에러코드로 변경
    //         }
    //     }

    //     return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUser(accessToken, membersUpdateDTO)));
    // }

    // @DeleteMapping
    // public ResponseEntity<BaseResponse<Members>> deleteUser(HttpServletRequest request) {
    //     String accessToken = request.getHeader(ACCESSTOKEN_HEADER).substring(7);
    //     return ResponseEntity.ok().body(new BaseResponse<>(memberService.deleteUser(accessToken)));
    // }

    @GetMapping("/universities")
    public ResponseEntity<BaseResponse<List<String>>> searchUniversities(
            @RequestParam("unvToSearch") String unvToSearch) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getUniversitiesList(unvToSearch)));
    }

    @GetMapping("/majors")
    public ResponseEntity<BaseResponse<List<String>>> searchMajors(
            @RequestParam("majorToSearch") String majorToSearch) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getMajorsList(majorToSearch)));
    }

    // @GetMapping
    // public ResponseEntity<BaseResponse<UserinfoDTO>> showUserinfos(HttpServletRequest request) {
    //     String accessToken = request.getHeader(ACCESSTOKEN_HEADER).substring(7);
    //     return ResponseEntity.ok().body(new BaseResponse<>(memberService.getUserinfo(accessToken)));
    // }
}
