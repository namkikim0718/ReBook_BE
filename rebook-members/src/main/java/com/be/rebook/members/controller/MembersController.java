package com.be.rebook.members.controller;

import com.be.rebook.members.dto.*;
import com.be.rebook.members.entity.Members;
import com.be.rebook.members.service.MemberService;

import org.springframework.http.ResponseEntity;
import com.be.rebook.common.argumentresolver.auth.Auth;
import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.config.BaseResponse;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequestMapping("/members")
public class MembersController {

    private final MemberService memberService;

    public MembersController(MemberService memberService) {
        this.memberService = memberService;
    }

    @GetMapping("/me")
    public ResponseEntity<BaseResponse<UserinfoDTO>> getMethodName(@Auth MemberLoginInfo memberLoginInfo) {
        UserinfoDTO info = memberService.getUserinfo(memberLoginInfo.getUsername());

        String picture = info.getStoredFileName();
        String nickname = info.getNickname();
        String unv = info.getUniversity();
        String majors = info.getMajors();

        if(picture == null)
            picture = "";
        if(nickname == null)
            nickname = "닉네임을 설정하세요.";
        if(unv == null)
            unv = "대학교를 설정하세요.";
        if(majors == null)
            majors = "관심 전공을 설정하세요.";

        return ResponseEntity.ok().body(new BaseResponse<>(UserinfoDTO.builder()
                .username(info.getUsername())
                .nickname(nickname)
                .storedFileName(picture)
                .university(unv)
                .majors(majors)
                .build()));
    }

    @GetMapping("/{username}")
    public ResponseEntity<BaseResponse<OtherUserinfoDTO>> getOtherUserinfo(@Auth MemberLoginInfo memberLoginInfo, @PathVariable String username){
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getOtherUserinfo(username)));
    }

    @PatchMapping("/nickname")
    public ResponseEntity<BaseResponse<Members>> updateUserNickname(@Auth MemberLoginInfo memberLoginInfo, @RequestBody UpdateNicknameDTO nicknameDTO){
        String username = memberLoginInfo.getUsername();
        String nicknameToUpdate = nicknameDTO.getNickname();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserNickname(username, nicknameToUpdate)));
    }

    @PatchMapping("/university")
    public ResponseEntity<BaseResponse<Members>> updateUserUniversity(@Auth MemberLoginInfo memberLoginInfo, @RequestBody UpdateUniversityDTO universityDTO){
        String username = memberLoginInfo.getUsername();
        String universityToUpdate = universityDTO.getUniversity();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserUniversity(username, universityToUpdate)));
    }

    @PatchMapping("/majors")
    public ResponseEntity<BaseResponse<Members>> updateUserMajors(@Auth MemberLoginInfo memberLoginInfo, @RequestBody UpdateMajorsDTO majorsDTO){
        String username = memberLoginInfo.getUsername();
        String majorsToUpdate = majorsDTO.getMajors();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserMajors(username, majorsToUpdate)));
    }

    @PatchMapping("/profilePicture")
    public ResponseEntity<BaseResponse<Members>> updateUserPicture(@Auth MemberLoginInfo memberLoginInfo, @RequestPart("picture") MultipartFile picture){
        String username = memberLoginInfo.getUsername();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserPicture(username, picture)));
    }

    @DeleteMapping("/profilePicture")
    public ResponseEntity<BaseResponse<Members>> deleteUserPicture(@Auth MemberLoginInfo memberLoginInfo){
        String username = memberLoginInfo.getUsername();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.deleteUserPicture(username)));
    }

    @DeleteMapping
    public ResponseEntity<BaseResponse<Members>> deleteUser(@Auth MemberLoginInfo memberLoginInfo) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.deleteUser(memberLoginInfo.getUsername())));
    }

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
}
