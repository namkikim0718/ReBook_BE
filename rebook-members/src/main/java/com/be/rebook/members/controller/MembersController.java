package com.be.rebook.members.controller;

import com.be.rebook.members.dto.UpdateMajorsDTO;
import com.be.rebook.members.dto.UpdateNicknameDTO;
import com.be.rebook.members.dto.UpdateUniversityDTO;
import com.be.rebook.members.entity.Members;
import com.be.rebook.members.service.MemberService;
import com.be.rebook.members.dto.UserinfoDTO;

import jakarta.validation.constraints.Pattern;

import org.springframework.http.ResponseEntity;

import com.be.rebook.common.argumentresolver.auth.Auth;
import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.config.BaseResponse;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import org.springframework.web.bind.annotation.RequestParam;

@RestController
@RequestMapping("/members")
public class MembersController {

    private final MemberService memberService;

    public MembersController(MemberService memberService) {
        this.memberService = memberService;
    }

    @GetMapping("/me")
    public ResponseEntity<BaseResponse<UserinfoDTO>> getMethodName(@Auth MemberLoginInfo memberLoginInfo) {
        UserinfoDTO userInfo = memberService.getUserinfo(memberLoginInfo.getUsername());
        return ResponseEntity.ok().body(new BaseResponse<>(userInfo));
    }

    @PatchMapping("/nickname")
    public ResponseEntity<BaseResponse<Members>> updateUserNickname(@Auth MemberLoginInfo memberLoginInfo, UpdateNicknameDTO nicknameDTO){
        String username = memberLoginInfo.getUsername();
        String nicknameToUpdate = nicknameDTO.getNickname();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserNickname(username, nicknameToUpdate)));
    }

    @PatchMapping("/university")
    public ResponseEntity<BaseResponse<Members>> updateUserUniversity(@Auth MemberLoginInfo memberLoginInfo, UpdateUniversityDTO universityDTO){
        String username = memberLoginInfo.getUsername();
        String universityToUpdate = universityDTO.getUniversity();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserUniversity(username, universityToUpdate)));
    }

    @PatchMapping("/majors")
    public ResponseEntity<BaseResponse<Members>> updateUserMajors(@Auth MemberLoginInfo memberLoginInfo, UpdateMajorsDTO majorsDTO){
        String username = memberLoginInfo.getUsername();
        String majorsToUpdate = majorsDTO.getMajors();
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.updateUserMajors(username, majorsToUpdate)));
    }

    //todo : 비밀번호 변경 로직 추가하기
    //todo : 프로필 사진 변경 로직 추가하기

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
            @Pattern(regexp = ".*[^가-힣\\sA-Z()].*", message = "검색어 입력이 잘못 되었습니다.") @RequestParam("majorToSearch") String majorToSearch) {
        return ResponseEntity.ok().body(new BaseResponse<>(memberService.getMajorsList(majorToSearch)));
    }
}
